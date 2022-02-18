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
      <returnable-list
        v-if="basicClass"
        ref="returnableListRef"
        :basicClass="basicClass"
        :select-list="selectList"
        :source-return-ids="sourceReturnIds"
        is-component
        @add="handleAdd"
        :edit="edit"
      />
    </template>
  </common-drawer>
</template>

<script setup>
import { ref, defineEmits, defineProps, defineExpose } from 'vue'
import useVisible from '@/composables/use-visible'
import ReturnableList from '../../returnable-list/index.vue'

const emit = defineEmits(['add', 'update:modelValue'])

const props = defineProps({
  edit: {
    type: Boolean,
    default: false
  },
  sourceReturnIds: {
    type: Array,
    default: () => []
  },
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
