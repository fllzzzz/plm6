<template>
  <common-dialog title="删除任务" v-model="dialogVisible" width="400px" :before-close="handleClose">
    <template #titleRight>
      <common-button :loading="submitLoading" type="primary" size="mini" @click="confirmIt">确 认</common-button>
    </template>
    <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="100px" @submit.prevent>
      <el-form-item label="编号">
        <span>{{ itemInfo.serialNumber }}</span>
      </el-form-item>
      <el-form-item label="规格">
        <span>{{ form.specification }}</span>
      </el-form-item>
      <el-form-item label="当前分配数量">
        <span>{{ itemInfo.schedulingQuantity }}</span>
      </el-form-item>
      <el-form-item label="删除数量" prop="quantity">
        <common-input-number
          v-model="form.quantity"
          :step="1"
          :min="1"
          :max="itemInfo.schedulingQuantity"
          :precision="0"
          size="small"
          controls-position="right"
          style="width: 250px"
        />
      </el-form-item>
      <el-form-item>
        <span class="form-item-tip">* 删除后，原数据退回到排产页面中。 </span>
      </el-form-item>
    </el-form>
  </common-dialog>
</template>

<script setup>
import { delRecord } from '@/api/mes/scheduling-manage/artifact'
import { defineEmits, defineProps, ref, reactive } from 'vue'
import { ElNotification } from 'element-plus'

import useVisible from '@compos/use-visible'

const emit = defineEmits(['update:visible', 'del-success'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  itemInfo: {
    type: Object,
    default: () => {}
  }
})

const { visible: dialogVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook })

const rules = {
  quantity: [{ required: true, message: '请填写删除数量', trigger: 'blur' }]
}
const formRef = ref()
const submitLoading = ref(false)
const form = reactive({ quantity: null })

function showHook() {
  form.quantity = props.itemInfo.schedulingQuantity
}

async function confirmIt() {
  try {
    submitLoading.value = true
    await delRecord([{ id: props.itemInfo.id, quantity: form.quantity }])
    ElNotification({
      title: '删除任务成功',
      type: 'success',
      duration: 2500
    })
    emit('del-success')
    handleClose()
  } catch (error) {
    console.log('删除任务失败', error)
  } finally {
    submitLoading.value = false
  }
}
</script>
