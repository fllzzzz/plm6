<template>
  <template v-if="boolNotBorrowReturn">
    <el-table-column v-if="!boolPublicTransferType" prop="partyATransferType" align="center" width="140px">
      <template #header>
        <common-radio-button
          type="enum"
          v-model="allPartyATransferType"
          :options="partyAMatTransferEnum.ENUM"
          @change="setAllPartyATransfer"
          size="mini"
        />
      </template>
      <template #default="{ row }">
        <common-radio-button
          v-if="row.sourceRow.boolPartyA"
          type="enum"
          v-model="row.sourceRow.partyATransferType"
          :options="partyAMatTransferEnum.ENUM"
          size="mini"
        />
        <span v-else>-</span>
      </template>
    </el-table-column>

    <el-table-column v-if="showPriceSet" prop="unitPrice" align="center" width="135px" label="含税单价">
      <template #default="{ row }">
        <common-input-number
          v-if="row.sourceRow.partyATransferType === partyAMatTransferEnum.BUY_IN.V"
          v-model="row.sourceRow.unitPrice"
          :min="0"
          :max="9999999"
          :controls="false"
          :step="1"
          size="mini"
          placeholder="含税单价"
          @change="handleUnitPriceChange($event, row.sourceRow)"
        />
        <span v-else>-</span>
      </template>
    </el-table-column>
    <el-table-column v-if="showPriceSet" prop="amount" align="center" width="135px" label="金额">
      <template #default="{ row }">
        <common-input-number
          v-if="row.sourceRow.partyATransferType === partyAMatTransferEnum.BUY_IN.V"
          v-model="row.sourceRow.amount"
          :min="0"
          :max="999999999"
          :controls="false"
          :step="1"
          :precision="2"
          size="mini"
          placeholder="金额"
          @change="handleAmountChange($event, row.sourceRow)"
        />
        <span v-else>-</span>
      </template>
    </el-table-column>
  </template>
</template>

<script setup>
import { ref, defineProps, watchEffect, computed } from 'vue'
import { isNotBlank, toPrecision } from '@/utils/data-type'
import { partyAMatTransferEnum, transferTypeEnum } from '@/utils/enum/modules/wms'
import { getDP } from '@/utils/data-type/number'
const props = defineProps({
  form: {
    type: Object,
    default: () => {
      return {
        list: []
      }
    }
  }
})

const formList = ref([])
// 调拨不是“借用归还的情况”
const boolNotBorrowReturn = computed(() => props.form.transferType !== transferTypeEnum.BORROW_RETURN.V)
// 默认甲供调拨类型：“借用”
const allPartyATransferType = ref(partyAMatTransferEnum.BORROW.V)
// 调拨到“公共库”时，不可借用
const boolPublicTransferType = computed(() => props.form.transferType === transferTypeEnum.PUBLIC_WARE.V)
// 显示价格设置
const showPriceSet = computed(() =>
  formList.value.some((row) => row.boolPartyA && row.partyATransferType === partyAMatTransferEnum.BUY_IN.V)
)

watchEffect(() => {
  // 监听list变化，并初始化甲供调拨类型
  formList.value = props.form.list || []
  // 公共库为买入
  if (boolPublicTransferType.value) {
    allPartyATransferType.value = boolPublicTransferType.value ? partyAMatTransferEnum.BUY_IN.V : partyAMatTransferEnum.BORROW.V
  }
  setAllPartyATransfer(allPartyATransferType.value)
})

// 处理全局甲供类型变化
function setAllPartyATransfer(val) {
  // 遍历设置甲供物料调拨类型
  formList.value.forEach((row) => {
    if (row.boolPartyA) {
      row.partyATransferType = val
    }
  })
}
// 处理含税单价变化
function handleUnitPriceChange(val, row) {
  const dp = getDP(val)
  if (dp > 10) {
    row.unitPrice = toPrecision(val, 10)
    val = row.unitPrice
  }
  row.amount = isNotBlank(val) ? toPrecision(val * row.mete, 2) : undefined
}

// 处理金额变化
function handleAmountChange(val, row) {
  row.unitPrice = isNotBlank(val) ? toPrecision(val / row.mete, 10) : undefined
}
</script>
