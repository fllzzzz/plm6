<template>
  <div>
    <table-tree
      :data-source="props.menus"
      :keys="props.menuIds"
      :props="{key: 'id', label: 'label', children: 'children'}"
      @change="onChange"
    />
  </div>

</template>

<script setup>
import { defineEmits, defineProps } from 'vue'
import TableTree from '@/components-system/system/vue-table-tree'

const props = defineProps({
  currentId: {
    type: Number,
    default: 0
  },
  menus: {
    type: Array,
    required: true
  },
  menuIds: {
    type: Array,
    default: () => []
  },
  permission: {
    type: Object,
    default: () => { }
  },
  roleName: {
    type: String,
    default: ''
  }
})
const emit = defineEmits(['updateSelect'])
function onChange(value) {
  const selectMenus = Array.from(value)
  emit('updateSelect', selectMenus)
}
</script>

