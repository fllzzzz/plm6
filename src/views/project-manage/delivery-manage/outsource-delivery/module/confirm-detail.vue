<template>
  <common-dialog
    ref="drawerRef"
    title="收货填报确认"
    append-to-body
    :close-on-click-modal="false"
    v-model="visible"
    :before-close="handleClose"
    width="500px"
  >
    <template #titleAfter>
      <!-- <el-tag type="warning" effect="plain" size="medium">分包公司：{{detailInfo.supplierName}}</el-tag> -->
    </template>
    <template #titleRight>
      <common-button type="danger" size="mini" @click="onCancel">取消</common-button>
      <common-button type="success" size="mini" @click="submitConfirm">确定收货</common-button>
    </template>
    <el-descriptions
      class="margin-top"
      border
      :column="1"
      size="large"
      style="width:400px"
    >
      <el-descriptions-item label="供应商">kooriookami</el-descriptions-item>
      <el-descriptions-item label="收货日期">18100000000</el-descriptions-item>
      <el-descriptions-item label="办理人">Suzhou</el-descriptions-item>
      <el-descriptions-item label="实收量">School</el-descriptions-item>
      <el-descriptions-item label="车牌号">
        <el-input
          v-model.trim="licenseNumber"
          type="text"
          style="width: 100%"
          maxlength="28"
          placeholder="车牌号"
        />
      </el-descriptions-item>
    </el-descriptions>
  </common-dialog>
</template>

<script setup>
import { ref, defineEmits, defineProps } from 'vue'

import useVisible from '@/composables/use-visible'
// import { ElMessageBox } from 'element-plus'

const emit = defineEmits(['update:modelValue'])

const props = defineProps({
  modelValue: {
    type: Boolean,
    require: true
  },
  detailInfo: {
    type: Object,
    default: () => {}
  },
  permission: {
    type: Object,
    default: () => {}
  }
})

const { visible, handleClose } = useVisible({ emit, props })
const licenseNumber = ref()
const drawerRef = ref()

function onCancel() {
  handleClose()
}

async function submitConfirm(val) {
  try {
    // const title = val === auditTypeEnum.PASS.V ? '通过' : '驳回'
    // const title = ''
    // const remarkValue = await ElMessageBox.prompt('请输入问题描述', title, {
    //   confirmButtonText: '确定',
    //   cancelButtonText: '取消',
    //   inputType: 'textarea',
    //   inputValidator: inputValid,
    //   type: 'warning'
    // })
    // const submitData = {
    //   auditStatus: val,
    //   id: props.detailInfo.id,
    //   remark: remarkValue.value
    // }
    // await confirmContract(submitData)
    // ElNotification({ title: '提交成功', type: 'success' })
    // emit('success')
    // handleClose()
  } catch (error) {
    console.log('审核', error)
  }
}
</script>
