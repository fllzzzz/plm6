<template>
  <common-dialog title="编辑零件数量" v-model="dialogVisible" width="400px" :show-close="false" :before-close="handleClose">
    <template #titleRight>
      <common-button v-loading="loading" @click="toConfirm" size="mini" type="primary">通过</common-button>
      <common-button @click="handleClose" size="mini">取消</common-button>
    </template>
    <el-form>
      <el-form-item label="零件编号">
        <span>{{ info.serialNumber }}</span>
      </el-form-item>
      <el-form-item label="构件编号">
        <span>{{ info.artifactSerialNumber }}</span>
      </el-form-item>
      <el-form-item label="零件数量" prop="quantity">
        <common-input-number
          v-model="form.quantity"
          :step="1"
          :min="0"
          :max="form.originQuantity"
          :precision="0"
          size="mini"
          style="width: 100%"
        />
      </el-form-item>
    </el-form>
  </common-dialog>
</template>

<script setup>
import { editUnProduct } from '@/api/mes/scheduling-manage/machine-part'
import { defineEmits, defineProps, ref } from 'vue'

import useVisible from '@compos/use-visible'
import { ElNotification, ElMessage } from 'element-plus'

const emit = defineEmits(['update:visible', 'refresh'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false,
  },
  info: {
    type: Object,
    default: () => ({}),
  },
})

const form = ref({})
const loading = ref(false)
const { visible: dialogVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook })

function showHook() {
  form.value = {
    id: props.info.id,
    quantity: props.info.quantity,
    originQuantity: props.info.quantity,
  }
}

async function toConfirm(row) {
  if (form.value.quantity === form.value.originQuantity) {
    ElMessage.warning('零件数量未修改')
    return
  }
  try {
    loading.value = true
    await editUnProduct(form.value)
    ElNotification({ type: 'success', title: '撤回成功' })
    handleClose()
    emit('refresh')
  } catch (error) {
    console.error(error)
  } finally {
    loading.value = false
  }
}
</script>
