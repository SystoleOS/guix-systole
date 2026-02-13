# SSH Deploy Key Auto-Injection - Implementation Summary

## Problem Solved

The SSH deploy key was being added as a **separate field** of `operating-system` instead of being included **inside** the `(services ...)` list, causing "invalid field specifier" error during `guix system init`.

## The Fix

### Modified: `system/installer/steps.scm`

Changed the approach from appending SSH service as a new field to **modifying the services field content**:

**Before:**
```scheme
(ssh-service-config
 (if (file-exists? deploy-key-file)
     (let ((deploy-key ...))
       (list `(service openssh-service-type ...)))
     '()))
(full-configuration (append configuration ssh-service-config))  ; WRONG: appends as new field
```

**After:**
```scheme
(ssh-service
 (if (file-exists? deploy-key-file)
     (let ((deploy-key ...))
       `(service openssh-service-type ...))  ; Single service, not list
     #f))
(full-configuration
 (if ssh-service
     (map (lambda (field)
            (match field
              (('services services-expr)
               `(services (cons* ,ssh-service ,services-expr)))  ; Inject into services
              (_ field)))
          configuration)
     configuration))
```

### Key Changes:

1. **`ssh-service-config` → `ssh-service`**: Single service expression, not a list
2. **Pattern matching with `match`**: Find the `(services ...)` field and modify it
3. **`cons*` to prepend**: Add SSH service to the front of the existing services list
4. **Map over configuration**: Transform the services field while preserving others

## Generated Configuration

### Before (Broken):
```scheme
(operating-system
  ...
  (services
   (cons (service dhcp-client-service-type) %base-services))  ; services field ends
  (service openssh-service-type                                ; separate field - ERROR!
           (openssh-configuration ...)))
```

### After (Correct):
```scheme
(operating-system
  ...
  (services
   (cons* (service openssh-service-type                        ; SSH inside services!
                   (openssh-configuration
                    (permit-root-login 'prohibit-password)
                    (password-authentication? #f)
                    (authorized-keys `(("root" ,(plain-file "deploy-key.pub" "ssh-ed25519 AAAA..."))))))
          (cons (service dhcp-client-service-type) %base-services))))
```

## Testing

### Offline Testing (Fast - No VM Required)

Run the comprehensive offline test:
```bash
./scripts/test-config-generation-offline.sh
```

This test:
1. ✅ Creates a test deploy key
2. ✅ Generates config using `systole-format-configuration`
3. ✅ Verifies SSH service is present **inside** services field
4. ✅ Validates Scheme syntax
5. ✅ Tests with `guix system build`

**Status**: ✅ All tests passing!

### Full Integration Testing (Slow - Requires VM)

Build installer ISO with deploy key:
```bash
# Generate test key if needed
ssh-keygen -t ed25519 -f /tmp/test-deploy-key -N "" -C "test"

# Build ISO (takes ~30-60 minutes)
./scripts/build-installer-with-deploy.sh --key-file /tmp/test-deploy-key.pub
```

Test in VM:
```bash
# Create test disk
qemu-img create -f qcow2 /tmp/test-systole.qcow2 20G

# Boot installer
qemu-system-x86_64 \
  -m 4096 -smp 2 \
  -boot d \
  -cdrom systole-installer-deploy-*.iso \
  -drive file=/tmp/test-systole.qcow2,format=qcow2 \
  -netdev user,id=net0,hostfwd=tcp::2223-:22 \
  -device e1000,netdev=net0 \
  -enable-kvm
```

Complete GUI installation, then after reboot:
```bash
# Verify SSH access
ssh -i /tmp/test-deploy-key -p 2223 root@localhost

# Verify SSH service in config
cat /etc/config.scm | grep -A 5 openssh-service-type
```

**Status**: ⏳ Pending (offline tests confirm code is correct)

## Files Modified

- ✅ `/home/rafael/src/guix-systole/system/installer/steps.scm` - Fixed SSH service generation
- ✅ `/home/rafael/src/guix-systole/system/installer/newt/systole-final.scm` - Already clean (no injection code)

## Obsolete Test Scripts

The following test scripts use the old post-processing approach and may be obsolete:
- `scripts/test-key-injection.scm` - Tests old string manipulation injection
- `scripts/test-ssh-service-generation.scm` - May need updating
- `scripts/test-installer-config-gen.scm` - May duplicate offline test

The canonical test is now: `scripts/test-config-generation-offline.sh`

## How It Works

1. **Build Time**: Deploy key is baked into installer ISO at `/etc/systole-deploy-key.pub`
2. **Installation Time**: When user completes GUI installation:
   - `systole-format-configuration` reads `/etc/systole-deploy-key.pub`
   - If present, generates SSH service S-expression
   - **Injects into services field** using `cons*` to prepend to existing services
   - Config written to `/mnt/etc/config.scm` in target system
3. **First Boot**: Installed system has SSH service with deploy key for root user

## Security Configuration

SSH service uses secure defaults:
- `permit-root-login 'prohibit-password` - Only key-based auth for root
- `password-authentication? #f` - Password auth disabled
- Deploy key authorizes only the `root` user
- Enables remote management with `guix deploy`

## Next Steps

1. ✅ **Done**: Offline testing validates config generation
2. ⏳ **Optional**: Build new ISO and test in VM for end-to-end validation
3. ⏳ **Optional**: Test `guix deploy` workflow with installed system
4. ⏳ **Cleanup**: Remove obsolete test scripts after confirmation

## Technical Notes

### Why `cons*` instead of `cons`?

`cons*` allows prepending multiple items:
```scheme
(cons* a b '(c d))  → (a b c d)
(cons a '(b c d))   → (a b c d)
```

Both work here since we're adding a single service, but `cons*` is more idiomatic when the tail might not be a proper list.

### Why `match` pattern matching?

Clean way to find and transform specific fields in the configuration list:
```scheme
(map (lambda (field)
       (match field
         (('services expr) `(services (cons* ,ssh-service ,expr)))
         (_ field)))
     configuration)
```

This preserves all other fields unchanged while modifying only the services field.
