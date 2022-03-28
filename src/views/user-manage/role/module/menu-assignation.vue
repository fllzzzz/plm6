<template>
  <div>
    <table-tree
      v-model="copyValue"
      checkable
      :return-leaf="false"
      :is-delete-nonexistent="false"
      return-indeterminate
      :options="props.menus"
      @change="handleChange"
    />
  </div>

</template>

<script setup>
import { ref, defineEmits, defineProps, watch } from 'vue'
import tableTree from '@/components-system/common/table-tree/index.vue'

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

const copyValue = ref([])
const emit = defineEmits(['updateSelect'])

watch(
  props.menuIds,
  (val) => {
    copyValue.value = val
  },
  { immediate: true, deep: true }
)

// 更新
function handleChange(value) {
  const selectMenus = Array.from(value)
  emit('updateSelect', selectMenus)
}
</script>

