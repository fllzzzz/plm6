<template>
  <common-drawer
    ref="drawerRef"
    v-model="visible"
    :before-close="handleClose"
    title="可退库清单"
    :show-close="true"
    size="80%"
    direction="btt"
    custom-class="returnable-list-drawer"
  >
    <template #content>
      <returnable-list ref="returnableListRef" :basicClass="basicClass" :select-list="selectList" is-component @add="handleAdd" />
    </template>
  </common-drawer>
</template>

<script setup>
import { ref, defineEmits, defineProps, defineExpose } from 'vue'
import useVisible from '@/composables/use-visible'
import ReturnableList from '../../returnable-list/index.vue'

const emit = defineEmits(['add', 'update:modelValue'])

const props = defineProps({
  modelValue: {
    type: Boolean,
    require: true
  },
  basicClass: {
    type: Number
  },
  selectList: {
    // 选中列表
    type: Array,
    default: () => []
  }
})

const { visible, handleClose } = useVisible({ emit, props })

// 可退库列表
const returnableListRef = ref()

function handleAdd(data) {
  emit('add', data)
}

function refresh() {
  returnableListRef.value && returnableListRef.value.refresh()
}

defineExpose({
  refresh
})
</script>
