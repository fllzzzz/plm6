<template>
  <div class="tree-item">
    <div class="tree-item-left">
      <table-checkbox :key="current[showprops.key]" :value="current[showprops.key]" :checked="current.checked" @on-click="onClick">{{ current[showprops.label] }}</table-checkbox>
    </div>
    <div v-if="current && current[showprops.children]" class="tree-item-right">
      <span v-if="!isList">
        <label v-for="c in current[showprops.children]" :key="c[showprops.key]">
          <table-checkbox :value="c[showprops.key]" :checked="c.checked" @on-click="onClick">{{ c[showprops.label] }}</table-checkbox>
        </label>
      </span>
      <table-item v-for="c in current[showprops.children]" v-else :key="c[showprops.key]" :item="c" :keys="keys" @on-click="onClick" />
    </div>
    <div class="tree-item-clear" />
  </div>
</template>
<script>
export default {
  name: 'TableItem'
}
</script>
<script setup>
import { defineEmits, defineProps, computed } from 'vue'
import TableCheckbox from './TableCheckBox'
const props = defineProps({
  item: {
    type: Object,
    default: () => ({})
  },
  keys: {
    type: Array,
    default: () => []
  }
})
const showprops = { key: 'id', label: 'label', children: 'children' }
const emit = defineEmits(['on-click'])
const isList = computed(() => {
  for (const item of props.item[showprops.children]) {
    if (item[showprops.children] && item[showprops.children].length > 0) {
      return true
    }
  }
  return false
})
const current = computed(() => {
  const keys = new Set([...props.keys])
  if (props.item[showprops.children]) {
    for (const c of props.item[showprops.children]) {
      c.checked = keys.has(c[showprops.key])
    }
  }
  const checked = keys.has(props.item[showprops.key])
  return { ...props.item, checked }
})
function onClick(state, value) {
  emit('on-click', state, value)
}
</script>
<style scoped>
  .tree-item{line-height: 50px; border-bottom: 1px solid #e5e5e5;}
  .tree-item-left{float: left; width: 200px;}
  .tree-item-right{float: right; width: calc(100% - 200px);border-left: 1px solid #e5e5e5;}
  .tree-item-right .tree-item:last-child{border-bottom: 0;}
  .tree-item label{margin-left: 10px; padding: 0; height: 30px; line-height: 30px;}
  .tree-item-clear{clear: both; margin: 0; padding: 0; height: 0;}
</style>
