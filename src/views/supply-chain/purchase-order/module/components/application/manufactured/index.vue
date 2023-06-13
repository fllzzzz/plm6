<template>
  <common-table
    :data="purchaseMergeList"
    return-source-data
    :show-empty-symbol="false"
    :max-height="maxHeight"
    :cell-class-name="wrongCellMask"
    style="width: 100%"
  >
    <el-table-column label="序号" type="index" align="center" width="60" />
    <el-table-column prop="name" label="名称" align="center" show-overflow-tooltip min-width="100px" />
    <el-table-column prop="specification" label="规格" align="center" show-overflow-tooltip min-width="140px" />
    <el-table-column prop="material" label="材质" align="center" show-overflow-tooltip />
    <el-table-column prop="length" label="总长度(m)" align="center" show-overflow-tooltip />
    <el-table-column prop="curPurchaseQuantity" label="数量" align="center" show-overflow-tooltip />
    <el-table-column prop="curPurchaseWeight" label="重量(kg)" align="center" show-overflow-tooltip />
    <el-table-column prop="pricingMethod" :show-overflow-tooltip="true" label="计量方式" width="160">
      <template #default="{ row }">
        <common-select
          v-model="row.pricingMethod"
          :options="wageQuotaTypeEnum.ENUM"
          type="enum"
          :unshowOptions="[wageQuotaTypeEnum.AREA.K, wageQuotaTypeEnum.QUANTITY.K]"
          placeholder="计量方式"
          style="width: 100%"
        />
      </template>
    </el-table-column>
    <!-- <el-table-column label="明细" align="center" show-overflow-tooltip min-width="100px">
      <template #default="{ row }">
        <common-button type="primary" size="mini" @click="showDetail(row)">查看</common-button>
      </template>
    </el-table-column> -->
    <el-table-column prop="unitPrice" align="center" width="135px" label="单价（元/吨）">
      <template #default="{ row }">
        <common-input-number
          v-model="row.unitPrice"
          :min="0"
          :max="9999999999"
          :controls="false"
          :step="1"
          size="mini"
          placeholder="单价"
        />
      </template>
    </el-table-column>
    <el-table-column prop="amount" align="center" width="135px" label="金额（元）" />
    <el-table-column prop="destination" align="center" width="135px" label="目的地">
      <template #default="{ row, $index }">
        <common-select
          v-model="row.destination"
          :options="destinationTypeEnum.ENUM"
          type="enum"
          size="mini"
          clearable
          :showExtra="$index !== 0"
          placeholder="目的地"
          style="width: 100%"
        />
      </template>
    </el-table-column>
    <el-table-column label="操作" align="center" width="70px">
      <template #default="{ row, $index }">
        <common-button type="danger" icon="el-icon-delete" size="mini" @click.stop="handleDel(row, $index)" />
      </template>
    </el-table-column>
  </common-table>
  <!-- <common-dialog
    title="清单明细"
    v-model="detailVisible"
    width="1200px"
    :show-close="true"
    custom-class="manufactured-order-purchase-detail-list-dialog"
    top="10vh"
  >
    <common-table :data="detailList" :max-height="dialogMaxHeight" style="width: 100%">
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column prop="monomer.name" label="单体" align="center" show-overflow-tooltip min-width="120px" />
      <el-table-column prop="area.name" label="区域" align="center" show-overflow-tooltip min-width="120px" />
      <el-table-column prop="name" label="名称" align="center" show-overflow-tooltip min-width="100px" />
      <el-table-column prop="serialNumber" label="编号" align="center" show-overflow-tooltip min-width="100px" />
      <el-table-column prop="specification" label="规格" align="center" show-overflow-tooltip min-width="140px" />
      <el-table-column label="采购数量" align="center" show-overflow-tooltip min-width="100px">
        <template #default="{ row: { sourceRow: row } }">
          <common-input-number
            v-if="row.isEdit"
            v-model="row.curPurchaseQuantity"
            :min="0"
            :max="row.canPurchaseQuantity"
            controls-position="right"
            :controls="false"
            :step="5"
            size="mini"
            placeholder="数量"
          />
          <span v-else>{{ row.sourceCurPurchaseQuantity }}</span>
        </template>
      </el-table-column>
      <el-table-column label="总重（kg）" align="center" show-overflow-tooltip>
        <template #default="{ row: { sourceRow: row } }">
          <span v-if="row.isEdit">{{ row.curPurchaseWeight }}</span>
          <span v-else>{{ row.sourceCurPurchaseWeight }}</span>
        </template>
      </el-table-column>
      <el-table-column label="操作" align="center" width="170px">
        <template #default="{ row: { sourceRow: row }, $index }">
          <common-button v-if="!row.isEdit" type="primary" icon="el-icon-edit" size="mini" @click.stop="row.isEdit = true" />
          <template v-else>
            <common-button type="success" icon="el-icon-check" size="mini" @click.stop="edit(row)" />
            <common-button type="info" icon="el-icon-close" size="mini" @click.stop="row.isEdit = false" />
          </template>
          <common-button type="danger" icon="el-icon-delete" size="mini" @click.stop="del(row, $index)" />
        </template>
      </el-table-column>
    </common-table>
  </common-dialog> -->
</template>

<script setup>
import { defineExpose, defineProps, computed, inject, watchEffect, watch } from 'vue'

// import { ElMessage } from 'element-plus'
import { wageQuotaTypeEnum } from '@enum-ms/mes'
import { destinationTypeEnum } from '@enum-ms/production'
import { manufClsEnum } from '@enum-ms/classification'
import { isNotBlank, toPrecision } from '@/utils/data-type'
import { getDP } from '@/utils/data-type/number'
import { DP } from '@/settings/config'
import useTableValidate from '@/composables/form/use-table-validate'
// import useMaxHeight from '@compos/use-max-height'

defineProps({
  maxHeight: {
    type: Number
  }
})

// 同上的选项与值
const ditto = new Map([['destination', -1]])

const tableRules = {
  unitPrice: [{ required: true, message: '请填写单价', trigger: 'blur' }],
  pricingMethod: [{ required: true, message: '请选择计价方式', trigger: 'change' }],
  destination: [{ required: true, message: '请选择目的地', trigger: 'change' }]
}

// 表格校验
const { tableValidate, cleanUpData, wrongCellMask } = useTableValidate({ rules: tableRules, ditto })

// const detailVisible = ref(false)
// const detailList = ref([])
// const detailRow = ref()
const form = inject('crud').form

// const { maxHeight: dialogMaxHeight } = useMaxHeight(
//   {
//     mainBox: '.manufactured-order-purchase-detail-list-dialog',
//     extraBox: ['.el-dialog__header'],
//     wrapperBox: ['.el-dialog__body'],
//     clientHRepMainH: true,
//     navbar: false
//   },
//   detailVisible
// )

const purchaseMergeList = computed(() => {
  const list = []
  // for (const item in form.manufMergeObj) {
  //   const _v = form.manufMergeObj[item]
  //   if (list.length > 0 && !_v?.destination) {
  //     _v.destination = -1
  //   }
  //   list.push(_v)
  //   rowWatch(_v)
  // }
  for (const item in form.manufListObj) {
    const _v = form.manufListObj[item]
    if (list.length > 0 && !_v?.destination) {
      _v.destination = -1
    }
    if (list.length === 0 && _v?.destination === -1) {
      _v.destination = undefined
    }
    list.push(_v)
    rowWatch(_v)
  }
  return list
})

watchEffect(() => {
  let _mete = 0
  let _amount = 0
  if (isNotBlank(purchaseMergeList.value)) {
    purchaseMergeList.value.forEach((v) => {
      _mete += v.curPurchaseWeight || 0
      _amount += Number(v.amount) || 0
    })
  }
  form.amount = toPrecision(_amount, DP.YUAN)
  form.mete = _mete
  form.meteUnit = 'kg'
})

function rowWatch(row) {
  watch([() => row.unitPrice, () => row.pricingMethod, () => row.mergeIds, () => row.destination], () => {
    // form.manufMergeObj[row.rowKey] = row
    form.manufListObj[row.rowKey] = row
  })
  watch([() => row.curPurchaseWeight, () => row.unitPrice, () => row.pricingMethod], () => {
    calcTotalAmount(row)
  })
}

function calcTotalAmount(row) {
  const dp = getDP(row.unitPrice)
  let _unitPrice = row.unitPrice
  if (dp > 10) {
    row.unitPrice = toPrecision(row.unitPrice, 10)
    _unitPrice = row.unitPrice
  }
  let _unitNet
  if (row.pricingMethod & wageQuotaTypeEnum.WEIGHT.V) {
    _unitNet = row.curPurchaseWeight
  }
  if (row.pricingMethod & wageQuotaTypeEnum.LENGTH.V) {
    _unitNet = row.length
  }
  row.amount = isNotBlank(_unitPrice) && isNotBlank(_unitNet) ? toPrecision(_unitPrice * (_unitNet / 1000), 2) : undefined
}

function handleDel(row, index) {
  delete form.manufListObj?.[row.rowKey]
}

// 处理金额变化
// function handleAmountChange(val, row) {
//   row.unitPrice = isNotBlank(val) ? toPrecision(val / (row.curPurchaseWeight / 1000), 10) : undefined
// }

// function showDetail(row) {
//   detailList.value = []
//   detailRow.value = row
//   for (let i = 0; i < row.mergeIds.length; i++) {
//     const id = row.mergeIds[i]
//     if (form.manufListObj?.[id]) {
//       const _v = form.manufListObj[id]
//       _v.isEdit = false
//       _v.sourceCurPurchaseQuantity = _v.curPurchaseQuantity
//       _v.sourceCurPurchaseWeight = _v.curPurchaseWeight
//       detailList.value.push(_v)
//       detailRowWatch(_v)
//     }
//   }
//   detailVisible.value = true
// }

// function detailRowWatch(row) {
//   watch(
//     () => row.curPurchaseQuantity,
//     () => {
//       row.curPurchaseWeight = toPrecision(row.curPurchaseQuantity * row.netWeight, 2) || 0
//     }
//   )
// }

// function del(row, index) {
//   detailList.value.splice(index, 1)
//   const _idx = detailRow.value.mergeIds.indexOf(row.id)
//   if (_idx !== -1) {
//     detailRow.value.mergeIds.splice(_idx, 1)
//     detailRow.value.curPurchaseQuantity -= row.curPurchaseQuantity
//     detailRow.value.curPurchaseWeight = toPrecision(detailRow.value.curPurchaseWeight - row.curPurchaseWeight, 2)
//   }
//   if (detailRow.value.mergeIds.length === 0) {
//     delete form.manufMergeObj?.[detailRow.value.rowKey]
//   }
//   delete form.manufListObj?.[row.id]
// }

// function edit(row, index) {
//   if (!row.curPurchaseQuantity) {
//     ElMessage.warning('采购数量不可修改为0')
//   }
//   detailRow.value.curPurchaseQuantity = detailRow.value.curPurchaseQuantity - row.sourceCurPurchaseQuantity + row.curPurchaseQuantity
//   detailRow.value.curPurchaseWeight = toPrecision(
//     detailRow.value.curPurchaseWeight - row.sourceCurPurchaseWeight + row.curPurchaseWeight,
//     2
//   )
//   row.sourceCurPurchaseQuantity = row.curPurchaseQuantity
//   row.sourceCurPurchaseWeight = row.curPurchaseWeight
//   form.manufListObj[row.id].curPurchaseQuantity = row.curPurchaseQuantity
//   form.manufListObj[row.id].curPurchaseWeight = row.curPurchaseWeight
//   row.isEdit = false
// }

function validate() {
  const { validResult } = tableValidate(purchaseMergeList.value)
  form.basicClass = manufClsEnum.STRUC_MANUFACTURED.V
  return validResult
}

function fetchResList() {
  return cleanUpData(purchaseMergeList.value)
}

defineExpose({
  validate,
  fetchResList
})
</script>

<style lang="scss" scoped></style>
