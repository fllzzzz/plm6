<template>
  <div style="display: flex; align-items: center; margin-bottom: 10px; justify-content: end">
    <el-checkbox
      v-model="checked"
      :label="`只查看填写处理数量的${componentTypeEnum.VL[productType]}`"
      @change="showEditList"
      style="margin-right: 10px"
    ></el-checkbox>
    <common-button type="primary" size="mini" @click="fill">一键填充</common-button>
    <common-button type="warning" size="mini" @click="reset">重置</common-button>
  </div>
</template>

<script setup>
import { defineProps, defineEmits, ref } from 'vue'
import { componentTypeEnum } from '@enum-ms/mes'

const emit = defineEmits(['showEditList', 'showAllList', 'fill', 'reset'])
defineProps({
  productType: {
    type: Number
  }
})

const checked = ref(false)

function showEditList(val) {
  if (val) {
    emit('showEditList')
  } else {
    emit('showAllList')
  }
}

function fill() {
  emit('fill')
}

function reset() {
  emit('reset')
}
</script>
