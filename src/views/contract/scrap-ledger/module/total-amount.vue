<template>
  <common-drawer v-model="visible" :before-close="handleClose" size="100%" title="累计出售额">
    <template #titleRight>
      <print-table
        api-key="totalScrapDetaile"
        :params="{ branchCompanyId: props.currentRow.branchCompanyId, contractWasteId: props.currentRow.contractWasteId }"
        v-permission="permission.detailPrint"
      />
    </template>
    <template #content>
      <common-table :data="tableData" :data-format="dataFormat">
        <el-table-column label="序号" align="center" width="60" type="index"></el-table-column>
        <el-table-column label="合同编号" align="center" prop="serialNumber"></el-table-column>
        <el-table-column label="购买单位" align="center" prop="purchaser"></el-table-column>
        <el-table-column label="废料类型" align="center" prop="wasteClassificationName"></el-table-column>
        <el-table-column label="核算单位" align="center" prop="measureUnit"></el-table-column>
        <el-table-column label="数量" align="center" prop="saleMete"></el-table-column>
        <el-table-column label="单价(元)" align="center" prop="price"></el-table-column>
        <el-table-column label="金额" align="center" prop="amount"></el-table-column>
        <el-table-column label="创建人" align="center" prop="createUserName"></el-table-column>
        <el-table-column label="出售日期" align="center" prop="saleDate"></el-table-column>
        <el-table-column label="审核人" align="center" prop="auditUserName"></el-table-column>
        <el-table-column label="审核日期" align="center" prop="auditTime"></el-table-column>
      </common-table>
    </template>
  </common-drawer>
</template>
<script setup>
import { defineProps, ref, defineEmits } from 'vue'
import { totalAmountList } from '@/api/contract/scrap-ledger'
import { scrapLedgerPM as permission } from '@/page-permission/contract'
import checkPermission from '@/utils/system/check-permission'
import useVisible from '@compos/use-visible'

const props = defineProps({
  modelValue: {
    type: Boolean,
    default: false
  },
  currentRow: {
    type: Object,
    default: () => {}
  }
})

const emit = defineEmits(['update:modelValue'])

const { visible, handleClose } = useVisible({ props, emit, showHook: fetchData })

const dataFormat = ref([
  ['createTime', ['parse-time', '{y}-{m}-{d}']],
  ['saleDate', ['parse-time', '{y}-{m}-{d}']],
  ['auditTime', ['parse-time', '{y}-{m}-{d}']]
])

const tableData = ref([])

async function fetchData() {
  if (!checkPermission(permission.detail)) return
  try {
    const { content } = await totalAmountList({
      branchCompanyId: props.currentRow.branchCompanyId,
      contractWasteId: props.currentRow.contractWasteId
    })
    console.log(content)
    tableData.value = content
  } catch (error) {
    console.log(error)
  }
}
</script>
