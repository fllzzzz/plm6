<template>
  <common-dialog
    title="运费变更提交预览"
    v-model="visible"
    top="10vh"
    append-to-body
    :before-close="handleClose"
    width="50%"
  >
    <template #titleRight>
      <common-button :loading="submitLoading" size="mini" :disabled="!props.modifiedData.length" type="primary" @click="submit">
        保 存
      </common-button>
    </template>
    <common-table :data="props.modifiedData" :max-height="maxHeight" empty-text="未做改动" style="width: 100%">
      <el-table-column prop="licensePlate" label="车牌号" align="center" show-overflow-tooltip />
      <el-table-column prop="loadingWeight" label="装载重量（吨）" align="center" show-overflow-tooltip />
      <el-table-column prop="purchaseSn" label="采购订单号" align="center" show-overflow-tooltip />
      <el-table-column prop="inboundSn" label="关联入库单" align="center" show-overflow-tooltip />
      <el-table-column align="center" prop="freight" label="运输费">
        <template #default="{ row }">
          <cell-change-preview :old="row.originFreight" :new="row.newFreight" />
        </template>
      </el-table-column>
    </common-table>
    <el-form style="margin-top:20px;">
      <el-form-item label="事由：">
        <el-input
          v-model.trim="remark"
          type="textarea"
          :autosize="{ minRows: 2, maxRows: 6}"
          placeholder="请输入事由"
          style="width: 100%;"
          show-word-limit
          maxlength="255"
        />
      </el-form-item>
    </el-form>
  </common-dialog>
</template>

<script setup>
import { feeChange } from '@/api/supply-chain/logistics-payment-manage/jd-logistics-record-ledger'
import { defineEmits, defineProps, ref } from 'vue'
import { ElNotification } from 'element-plus'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
import cellChangePreview from '@comp-common/cell-change-preview'

const emit = defineEmits(['success', 'update:visible'])
const props = defineProps({
  modifiedData: {
    type: Array,
    default: () => []
  },
  modelValue: {
    type: Boolean,
    require: true
  }
})

const submitLoading = ref(false)
const remark = ref()

const { visible, handleClose } = useVisible({ emit, props })
const { maxHeight } = useMaxHeight(
  {
    extraBox: ['.el-dialog__header'],
    wrapperBox: ['.el-dialog__body'],
    clientHRepMainH: true,
    minHeight: 300,
    navbar: false,
    extraHeight: 150
  },
  visible
)

// 提交
async function submit() {
  try {
    submitLoading.value = true
    const _list = []
    props.modifiedData.map((v) => {
      _list.push({
        id: v.id,
        freight: v.newFreight,
        reason: remark.value
      })
    })
    await feeChange(_list)
    ElNotification({ title: '提交成功', type: 'success' })
    handleClose()
    emit('success')
  } catch (error) {
    console.log('提交原材料物流费价格', error)
  } finally {
    submitLoading.value = false
  }
}
</script>
