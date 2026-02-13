# SSH Deploy Key Auto-Injection - Final Implementation

## Summary

Implemented automatic SSH deploy key injection that **only activates when the user selects SSH service** in the GUI installer. Uses `modify-services` to enhance the existing SSH configuration rather than adding a duplicate service.

## How It Works

### User Workflow
1. **Build installer with deploy key**:
   ```bash
   ./scripts/build-installer-with-deploy.sh --key-file ~/.ssh/id_ed25519.pub
   ```

2. **Use GUI installer normally**: Complete installation, optionally select "SSH Server" in services step

3. **Two scenarios**:
   - **SSH selected**: Deploy key automatically added to root's authorized_keys
   - **SSH NOT selected**: No SSH service added (respects user choice)

### Implementation Logic

**File**: `system/installer/steps.scm`

```scheme
(define (systole-format-configuration steps results)
  (let* ((configuration ...)  ; Generated from installer steps

         ;; Check for deploy key
         (deploy-key
          (and (file-exists? "/etc/systole-deploy-key.pub")
               (string-trim-right
                (call-with-input-file "/etc/systole-deploy-key.pub"
                  get-string-all))))

         ;; Helper to detect if SSH service exists
         (ssh-service-exists?
          (lambda (services-expr)
            (string-contains (format #f "~a" services-expr)
                           "openssh-service-type")))

         ;; Enhance SSH config if both deploy key AND SSH service present
         (final-configuration
          (if deploy-key
              (map (lambda (field)
                     (match field
                       (('services services-expr)
                        (if (ssh-service-exists? services-expr)
                            ;; Wrap with modify-services
                            `(services (modify-services ,services-expr
                                         (openssh-service-type config =>
                                           (openssh-configuration
                                            (inherit config)
                                            (authorized-keys
                                             (cons `("root" ,(plain-file "deploy-key.pub"
                                                                         ,deploy-key))
                                                   (openssh-configuration-authorized-keys config)))))))
                            ;; SSH not selected, don't modify
                            field))
                       (_ field)))
                   configuration)
              configuration)))
    ...))
```

### Generated Configuration

**When SSH selected + deploy key present**:
```scheme
(operating-system
  ...
  (services
   (modify-services (cons* (service openssh-service-type)
                           (service dhcp-client-service-type)
                           %base-services)
     (openssh-service-type config =>
       (openssh-configuration
        (inherit config)
        (authorized-keys
         (cons `("root" ,(plain-file "deploy-key.pub" "ssh-ed25519 AAAA..."))
               (openssh-configuration-authorized-keys config))))))))
```

**When SSH NOT selected**:
```scheme
(operating-system
  ...
  (services
   (cons (service dhcp-client-service-type)
         %base-services)))
```

## Key Design Decisions

### 1. Only Enhance, Never Force
- **Rationale**: Respects user choice - if they don't want SSH, we don't force it
- **Implementation**: Check if `openssh-service-type` exists before modifying

### 2. Use `modify-services` Instead of Adding New Service
- **Rationale**: Prevents "service provided more than once" error
- **Benefit**: Preserves any custom SSH configuration user might have set
- **Pattern**: Standard Guix way to customize service configurations

### 3. Inherit Existing Configuration
- **Rationale**: Don't overwrite settings user configured in GUI
- **Implementation**: `(inherit config)` preserves all fields
- **Additive**: `cons` deploy key to existing authorized-keys list

## Testing

### Offline Testing (Fast)
```bash
./scripts/test-config-generation-offline.sh
```

**Tests**:
- ✅ Config generation with SSH selected
- ✅ Deploy key properly injected via `modify-services`
- ✅ Valid Scheme syntax
- ✅ Successful `guix system build`

### Manual Testing Scenarios

**Scenario 1: SSH Selected + Deploy Key**
1. Build ISO with deploy key
2. Select "SSH Server" in services step
3. Complete installation
4. **Expected**: `/etc/config.scm` contains `modify-services` with deploy key
5. **Verify**: `ssh -i <key> root@<host>` works without password

**Scenario 2: SSH NOT Selected + Deploy Key**
1. Build ISO with deploy key
2. DO NOT select "SSH Server" in services step
3. Complete installation
4. **Expected**: `/etc/config.scm` has no SSH service
5. **Verify**: SSH daemon not running (`herd status ssh-daemon` fails)

**Scenario 3: SSH Selected + NO Deploy Key**
1. Build ISO normally (no deploy key)
2. Select "SSH Server" in services step
3. Complete installation
4. **Expected**: `/etc/config.scm` has SSH service, no deploy key modifications
5. **Verify**: Standard SSH configuration without authorized_keys

## Security Considerations

- **Deploy key only for root**: Enables `guix deploy` remote management
- **Preserves SSH settings**: Doesn't override security configurations
- **Key-based auth**: Deploy key uses public key authentication
- **Optional feature**: Only active when user both:
  1. Builds ISO with deploy key
  2. Selects SSH service in installer

## Files Modified

- ✅ `system/installer/steps.scm` - Core implementation
- ✅ `scripts/test-config-generation-offline.sh` - Updated test to include SSH

## Migration from Previous Approach

**Old approach (removed)**:
- Post-processed generated config.scm with string manipulation
- Fragile pattern matching on `%base-services)))`
- Added SSH service even when user didn't select it

**New approach (current)**:
- Generates correct configuration from the start
- Uses proper S-expression manipulation
- Only enhances SSH when explicitly selected
- No string manipulation or regex

## Usage Examples

### For Development/Testing
```bash
# Generate test key
ssh-keygen -t ed25519 -f /tmp/deploy-key -N "" -C "deploy"

# Build installer with key
./scripts/build-installer-with-deploy.sh --key-file /tmp/deploy-key.pub

# Test in VM
qemu-system-x86_64 \
  -m 4096 -smp 2 \
  -boot d -cdrom systole-installer-deploy-*.iso \
  -drive file=test.qcow2,format=qcow2 \
  -netdev user,id=net0,hostfwd=tcp::2223-:22 \
  -device e1000,netdev=net0 \
  -enable-kvm
```

### For Production Deployment
```bash
# Use your actual deploy key
./scripts/build-installer-with-deploy.sh --key-file ~/.ssh/guix_deploy.pub

# After installation, verify
ssh -i ~/.ssh/guix_deploy root@<installed-host>

# Use with guix deploy
guix deploy deployment.scm
```

## Troubleshooting

### Deploy key not working
1. Check if SSH was selected during installation: `grep openssh /etc/config.scm`
2. Verify key is in config: `grep deploy-key.pub /etc/config.scm`
3. Check SSH daemon: `herd status ssh-daemon`
4. Verify authorized_keys: `cat /root/.ssh/authorized_keys`

### "Service provided more than once" error
- **Should not occur** with current implementation
- If it does, verify only one of these is true:
  - User selected SSH in GUI
  - OR deploy key triggers injection
- Check for duplicate `openssh-service-type` in config

## Future Enhancements

- Support for multiple authorized keys
- Deploy key for non-root users
- SSH configuration options in installer GUI
- Deploy key rotation mechanism

## References

- GNU Guix manual: [Service Reference](https://guix.gnu.org/manual/en/html_node/Services.html)
- `modify-services` pattern: [Service Customization](https://guix.gnu.org/manual/en/html_node/Service-Reference.html#index-modify_002dservices)
- openssh-service-type: [SSH Services](https://guix.gnu.org/manual/en/html_node/Networking-Services.html#SSH-Services)
