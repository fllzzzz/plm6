<template>
  <el-dialog
    append-to-body
    :close-on-click-modal="false"
    v-model="visible"
    width="80%"
    fullscreen
    :show-close="false"
    custom-class="dialog-class"
  >
    <common-button type="danger" icon="el-icon-close" circle class="btn-close" @click="handleClose" />
    <simpleJpg ref="draw" @download="saveJPG" />
  </el-dialog>
</template>

<script setup>
import { saveDraw } from '@/api/plan/technical-manage/enclosure'
import { defineProps, defineEmits, ref } from 'vue'
import useVisible from '@compos/use-visible'
import simpleJpg from './simpleJpg'
import { ElMessage } from 'element-plus'
const draw = ref()
const props = defineProps({
  action: { // create表单新增  override编辑覆盖
    type: String,
    default: 'create',
    validator: function (val) {
      return ['create', 'override'].indexOf(val) !== -1
    }
  },
  modelValue: {
    type: Boolean,
    require: true
  },
  current: {
    type: Object,
    default: () => {}
  }
})

const emit = defineEmits(['toQuery', 'success'])
const { visible, handleClose } = useVisible({ emit, props })

async function saveJPG(base64) {
  try {
    const data = {
      fileName: `${props.current.serialNumber}.jpg`,
      img: base64,
      id: props.current.id
    }
    await saveDraw(data)
    ElMessage({
      message: '保存成功',
      type: 'success'
    })
    handleClose()
  } catch (err) {
    console.log('图纸变更', err)
  } finally {
    emit('toQuery')
  }
}
</script>
<style lang="scss" scoped>

.btn-close{
  position: absolute;
  top: 20px;
  right: 30px;
  z-index: 2;
}
</style>
<style lang="scss">
.dialog-class{
  .el-dialog__header {
    padding: 0px;
  }
  .el-dialog__body {
    padding: 0px;
  }
}
</style>
