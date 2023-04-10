<template>
  <common-dialog
    :title="`${contractSaleTypeEnum.V[props.params.type]?.SL}价格修改`"
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
      <el-table-column label="序号" type="index" align="center" width="60" />
      <template v-if="props.params.type === contractSaleTypeEnum.STRUCTURE.V || props.params.type === contractSaleTypeEnum.MACHINE_PART.V">
        <el-table-column prop="name" label="名称" align="center" />
        <el-table-column prop="material" label="材质" align="center" />
        <el-table-column prop="totalQuantity" label="数量" align="center" />
        <el-table-column align="center" prop="pricingManner" label="计价方式">
          <template #default="{ row }">
            <span v-if="row.pricingManner === row.originPricingManner">{{ pricingMannerEnum.VL[row.originPricingManner] }}</span>
            <cell-change-preview :old="pricingMannerEnum.VL[row.originPricingManner]" :new="pricingMannerEnum.VL[row.pricingManner]" v-else />
          </template>
        </el-table-column>
      </template>
      <!-- <template v-if="props.params.type === contractSaleTypeEnum.ENCLOSURE.V">
        <el-table-column prop="name" label="名称" align="center" />
        <el-table-column prop="plate" label="板型" align="center" />
        <el-table-column prop="totalQuantity" label="数量" align="center" />
      </template> -->
      <template v-if="props.params.type === contractSaleTypeEnum.AUXILIARY_MATERIAL.V">
        <el-table-column prop="classifyName" label="名称" align="center" />
        <el-table-column prop="specification" label="规格" align="center" />
        <el-table-column prop="mete" label="核算量" align="center" />
      </template>
      <el-table-column align="center" prop="price" label="综合单价">
        <template #default="{ row }">
          <template v-if="props.params.type === contractSaleTypeEnum.STRUCTURE.V">
            <span v-if="row.originUnitPrice === row.unitPrice">{{ row.unitPrice }}</span>
            <cell-change-preview :old="row.originUnitPrice" :new="row.unitPrice" v-else/>
          </template>
          <cell-change-preview :old="row.originUnitPrice" :new="row.unitPrice" v-else/>
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
import { save } from '@/api/contract/sales-manage/price-manage/common'
import { defineEmits, defineProps, ref, useAttrs } from 'vue'
import { ElNotification } from 'element-plus'

import { contractSaleTypeEnum } from '@enum-ms/mes'
import { pricingMannerEnum } from '@enum-ms/contract'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
import cellChangePreview from '@comp-common/cell-change-preview'

const emit = defineEmits(['success', 'update:visible'])
const props = defineProps({
  modifiedData: {
    type: Array,
    default: () => []
  },
  params: {
    type: Object,
    default: () => {}
  },
  modelValue: {
    type: Boolean,
    require: true
  }
})

const submitLoading = ref(false)
const remark = ref()

const attrs = useAttrs()
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
      if (props.params.type === contractSaleTypeEnum.STRUCTURE.V || props.params.type.type === contractSaleTypeEnum.MACHINE_PART.V) {
        _list.push({
          id: v.id,
          unitPrice: v.newUnitPrice ? v.newUnitPrice : (v.unitPrice !== '-' ? v.unitPrice : null),
          pricingManner: props.params.type === contractSaleTypeEnum.STRUCTURE.V ? v.pricingManner : undefined
        })
      } else {
        _list.push({
          id: v.id,
          unitPrice: v.newUnitPrice
        })
      }
    })
    await save({
      details: _list,
      remark: remark.value,
      ...props.params
    })
    ElNotification({ title: '提交成功', type: 'success' })
    handleClose()
    emit('success')
    // 刷新待审核数量
    attrs.onRefreshCount()
  } catch (error) {
    console.log('提交商务价格', error)
  } finally {
    submitLoading.value = false
  }
}
</script>
