<template>
  <common-dialog
    :title="`${globalProject?.projectType === projectTypeEnum.BRIDGE.V ? '分段' : contractSaleTypeEnum.V[props.params.type]?.SL}价格修改`"
    v-model="visible"
    top="8vh"
    append-to-body
    :before-close="handleClose"
    width="50%"
  >
    <template #titleRight>
      <common-button :loading="submitLoading" size="mini" :disabled="!props.modifiedData.length" type="primary" @click="submit">
        保 存
      </common-button>
    </template>
    <common-table :data="props.modifiedData" :max-height="maxHeight - 180" empty-text="未做改动" style="width: 100%">
      <el-table-column label="序号" type="index" align="center" width="60" />
      <template
        v-if="
          globalProject?.projectType !== projectTypeEnum.BRIDGE.V &&
          (props.params.type === contractSaleTypeEnum.STRUCTURE.V || props.params.type === contractSaleTypeEnum.MACHINE_PART.V)
        "
      >
        <el-table-column prop="name" label="名称" align="center" />
        <el-table-column prop="specification" label="规格" align="center" />
        <el-table-column prop="material" label="材质" align="center" />
        <el-table-column prop="totalQuantity" label="数量" align="center" />
        <el-table-column align="center" prop="pricingManner" label="计价方式">
          <template #default="{ row }">
            <span v-if="row.pricingManner === row.originPricingManner">{{ pricingMannerEnum.VL[row.originPricingManner] }}</span>
            <cell-change-preview
              :old="pricingMannerEnum.VL[row.originPricingManner]"
              :new="pricingMannerEnum.VL[row.pricingManner]"
              v-else
            />
          </template>
        </el-table-column>
      </template>
      <template v-if="props.params.type === contractSaleTypeEnum.ENCLOSURE.V">
        <el-table-column prop="name" label="名称" align="center" />
        <el-table-column v-if="props.categoryValue !== mesEnclosureTypeEnum.FOLDING_PIECE.V" prop="plate" label="板型" align="center" />
        <el-table-column prop="specification" label="规格" align="center" />
        <el-table-column prop="totalQuantity" label="数量" align="center" />
        <el-table-column align="center" prop="pricingManner" label="计价方式">
          <template #default="{ row }">
            <span>{{ enclosureSettlementTypeEnum.VL[row.pricingManner] }}</span>
          </template>
        </el-table-column>
      </template>
      <template v-if="props.params.type === contractSaleTypeEnum.AUXILIARY_MATERIAL.V">
        <el-table-column prop="name" label="名称" align="center" />
        <el-table-column prop="specification" label="规格" align="center" />
        <el-table-column prop="quantity" label="数量" align="center" />
      </template>
      <el-table-column align="center" prop="price" label="综合单价">
        <template #default="{ row }">
          <template v-if="props.params.type === contractSaleTypeEnum.STRUCTURE.V">
            <span v-if="row.originUnitPrice === row.unitPrice">{{ row.unitPrice }}</span>
            <cell-change-preview :old="row.originUnitPrice === '同上' ? '-' : row.originUnitPrice" :new="row.unitPrice" v-else />
          </template>
          <cell-change-preview :old="row.originUnitPrice === '同上' ? '-' : row.originUnitPrice" :new="row.unitPrice" v-else />
        </template>
      </el-table-column>
    </common-table>
    <el-form style="margin-top: 20px" v-if="priceEditMode === priceEditModeEnum.AUDIT.V || showType === 'log'">
      <el-form-item label="事由：">
        <el-input
          v-model.trim="remark"
          type="textarea"
          :autosize="{ minRows: 2, maxRows: 6 }"
          placeholder="请输入事由"
          style="width: 100%"
          show-word-limit
          maxlength="255"
        />
      </el-form-item>
    </el-form>
  </common-dialog>
</template>

<script setup>
import { save, saveTempPrice } from '@/api/contract/sales-manage/price-manage/common'
import { saveStandardPart } from '@/api/contract/sales-manage/price-manage/auxiliary-material'
import { defineEmits, defineProps, ref, useAttrs, inject } from 'vue'
import { ElNotification } from 'element-plus'

import { contractSaleTypeEnum, mesEnclosureTypeEnum } from '@enum-ms/mes'
import { enclosureSettlementTypeEnum, pricingMannerEnum, priceEditModeEnum, projectTypeEnum } from '@enum-ms/contract'

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
  },
  categoryValue: {
    type: Number
  },
  showType: {
    type: String,
    default: undefined
  }
})

const globalProject = inject('globalProject')

const submitLoading = ref(false)
const remark = ref()
const priceEditMode = inject('priceEditMode')

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
    if (props.showType === 'log') {
      props.modifiedData.map((v) => {
        _list.push(v.id)
      })
    } else {
      props.modifiedData.map((v) => {
        if (props.params.type !== contractSaleTypeEnum.AUXILIARY_MATERIAL.V) {
          _list.push({
            id: v.id,
            unitPrice: v.unitPrice !== '同上' ? v.unitPrice : null,
            pricingManner: v.pricingManner
          })
        } else {
          _list.push({
            id: v.id,
            unitPrice: v.unitPrice !== '同上' ? v.unitPrice : null
          })
        }
      })
    }
    const api =
      props.showType === 'log' ? saveTempPrice : props.params.type === contractSaleTypeEnum.AUXILIARY_MATERIAL.V ? saveStandardPart : save
    const params =
      props.showType === 'log'
        ? { priceTempIds: _list, remark: remark.value, ...props.params }
        : { details: _list, remark: remark.value, ...props.params }
    await api(params)
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
