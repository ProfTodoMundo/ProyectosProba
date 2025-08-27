#!/bin/bash

MODELOS=(
    auto
    generic
    alc233
    dell-headset-multi
    laptop
    laptop-amic
    laptop-dmic
    intel
)

echo "🎯 Iniciando prueba de modelos para snd-hda-intel..."

for modelo in "${MODELOS[@]}"; do
    echo "🔧 Probando modelo: $modelo"

    # Modificar configuración
    echo "options snd-hda-intel model=$modelo" | sudo tee /etc/modprobe.d/alsa-base.conf > /dev/null

    # Regenerar initramfs
    sudo update-initramfs -u > /dev/null

    # Recargar módulos de ALSA
    sudo alsa force-reload > /dev/null

    sleep 2

    # Verificar si ahora se detecta la tarjeta de sonido
    if aplay -l | grep -q "card"; then
        echo "✅ ¡Éxito! Modelo que funciona: $modelo"
        aplay -l
        exit 0
    else
        echo "❌ Modelo $modelo no funcionó."
    fi
done

echo "🚫 Ninguno de los modelos probados funcionó. Puede que necesites un modelo menos común o revisar BIOS/UEFI."
