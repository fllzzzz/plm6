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
      <returnable-list :basicClass="basicClass" :select-list="selectList" is-component />
    </template>
  </common-drawer>
</template>

<script setup>
import { defineEmits, defineProps } from 'vue'
import useVisible from '@/composables/use-visible'
import ReturnableList from '../../returnable-list/index.vue'

const emit = defineEmits(['success', 'update:modelValue'])

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

</script>
