<template>
  <common-drawer v-model="visible" :before-close="handleClose" size="70%" title="废料出售单详情">
    <template #titleRight>
      <export-button :fn="downloadScrap" :params="props.rowData.id" v-permission="permission.download">下载</export-button>
    </template>
    <template #content>
      <el-descriptions :column="2" border style="margin-bottom: 20px;">
        <el-descriptions-item label="创建日期">{{ form?.createTime }}</el-descriptions-item>
        <el-descriptions-item label="创建人">{{ form.createUserName }}</el-descriptions-item>
        <el-descriptions-item label="出售单位">{{ form.branchCompanyName }}</el-descriptions-item>
        <el-descriptions-item label="购买单位">{{ form.purchaser }}</el-descriptions-item>
        <el-descriptions-item label="合同编号">{{ form.serialNumber }}</el-descriptions-item>
        <el-descriptions-item label="车牌号">{{ form.plateNumber }}</el-descriptions-item>
      </el-descriptions>
      <div style="display: flex; flex-direction: column; align-items: center">
        <common-table :data="tableData" returnSourceData>
          <el-table-column label="序号" align="center" width="55" type="index"></el-table-column>
          <el-table-column label="类型" align="center" width="150">
            <template #default="{ row }">
              <span>{{ row.wasteClassificationName }}</span>
            </template>
          </el-table-column>
          <el-table-column label="核算单位" align="center" width="80">
            <template #default="{ row }">
              <span>{{ row.measureUnit }}</span>
            </template>
          </el-table-column>
          <el-table-column label="数量" align="center" width="100">
            <template #default="{ row }">
              <span>{{ row.saleMete }}</span>
            </template>
          </el-table-column>
          <el-table-column label="单价" align="center" width="125">
            <template #default="{ row }">
              <span>{{ row.price }}</span>
            </template>
          </el-table-column>
          <el-table-column label="金额" align="center" width="125">
            <template #default="{ row }">
              <span>{{ row.amount }}</span>
            </template>
          </el-table-column>
          <el-table-column label="备注" align="center">
            <template #default="{ row }">
              <span>{{ row.remark }}</span>
            </template>
          </el-table-column>
        </common-table>
      </div>
      <div style="margin-top: 20px; display: flex; align-items: center;">
        <span style="font-size: 16px; font-weight: bold;">创建备注：</span>
        <el-input style="width: 750px;" type="textarea" maxlength="200" :disabled="editBoolean" v-model="creatRemark" />
      </div>
      <div style="margin-top: 20px; display: flex; align-items: center;">
        <span style="font-size: 16px; font-weight: bold;">审核备注：</span>
        <el-input style="width: 750px;" type="textarea" maxlength="200" :disabled="editBoolean" v-model="checkRemark" />
      </div>
    </template>
  </common-drawer>
</template>
<script setup>
import { defineProps, defineEmits, ref } from 'vue'
import useVisible from '@compos/use-visible'
import ExportButton from '@comp-common/export-button/index.vue'
import { downloadScrap } from '@/api/wms/scrap-sell'
import { scrapSellPM as permission } from '@/page-permission/wms'

const props = defineProps({
  modelValue: {
    type: Boolean,
    default: false
  },
  rowData: {
    type: Object,
    default: () => {}
  }
})

const emit = defineEmits(['update:modelValue', 'success'])
const { visible, handleClose } = useVisible({ props, emit, showHook: fetchData })

const defaultForm = {
  branchCompanyName: undefined,
  contractWasteId: undefined,
  creatorRemark: undefined,
  id: undefined,
  plateNumber: undefined,
  saleDate: new Date().getTime().toString(),
  contractNumber: undefined,
  wasteSaleDetailList: []
}

const form = ref(JSON.parse(JSON.stringify(defaultForm)))
const tableData = ref([])
const checkRemark = ref()
const creatRemark = ref()
const editBoolean = ref(true)

async function fetchData() {
  tableData.value = []
  form.value = props.rowData
  creatRemark.value = props.rowData.creatorRemark
  checkRemark.value = props.rowData.auditorRemark
  console.log(props.rowData.wasteSaleDetailList)
  props.rowData.wasteSaleDetailList.forEach(v => {
    if (v?.id) {
      tableData.value.push(v)
    }
  })
}

</script>
