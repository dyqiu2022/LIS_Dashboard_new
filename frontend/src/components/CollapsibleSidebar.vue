<script setup lang="ts">
const collapsed = defineModel<boolean>({ default: false })

withDefaults(defineProps<{
  title: string
  eyebrow?: string
  icon?: string
  tone?: string
}>(), {
  eyebrow: 'CONTROLS',
  icon: '⌁',
  tone: '',
})
</script>

<template>
  <aside class="collapsible-sidebar" :class="[{ 'is-collapsed': collapsed }, tone ? `sidebar-tone-${tone}` : '']">
    <div class="collapsible-sidebar-heading">
      <div class="collapsible-sidebar-identity">
        <span class="sidebar-icon">{{ icon }}</span>
        <div v-if="!collapsed" class="sidebar-heading-copy">
          <span class="section-kicker">{{ eyebrow }}</span>
          <strong>{{ title }}</strong>
        </div>
      </div>
      <button
        type="button"
        class="sidebar-toggle"
        :aria-expanded="!collapsed"
        :aria-label="collapsed ? `展开${title}` : `收起${title}`"
        @click="collapsed = !collapsed"
      >
        <span>{{ collapsed ? '→' : '←' }}</span>
        <em>{{ collapsed ? '展开' : '收起' }}</em>
      </button>
    </div>
    <div v-if="!collapsed" class="collapsible-sidebar-body">
      <slot />
    </div>
  </aside>
</template>
