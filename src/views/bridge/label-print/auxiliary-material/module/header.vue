<template>
  <div v-show="crud.searchToggle">
    <monomer-select
      v-model="query.monomerId"
      :project-id="globalProjectId"
      :default="false"
      clearable
      class="filter-item"
      @change="crud.toQuery"
    />
    <material-cascader
      v-model="query.classifyId"
      :basic-class="rawMatClsEnum.MATERIAL.V "
      separator=" > "
      check-strictly
      show-all-levels
      clearable
      size="small"
      class="filter-item"
      style="width: 300px"
      placeholder="可选择/输入科目、编号搜索"
      @change="crud.toQuery"
    />
    <!-- <el-input
      v-model="query.classifyName"
      size="small"
      placeholder="名称搜索"
      style="width: 170px; margin-left: 0"
      class="filter-item"
      clearable
      @blur="crud.toQuery"
    /> -->
    <rrOperation />
  </div>
  <crudOperation>
    <template #optRight>
      <el-popover v-model:visible="printConfigVisible" placement="bottom-start" width="400">
        <el-form ref="form" :model="printConfig" label-width="90px" size="mini">
          <el-form-item label="份数">
            <el-input-number
              v-model="printConfig.copiesQuantity"
              :step="1"
              :min="1"
              :max="99"
              style="width: 250px"
              @change="handleCopiesChange"
            />
          </el-form-item>
        </el-form>
        <template #reference>
          <common-button type="primary" size="mini">打印设置</common-button>
        </template>
        <div style="text-align: right">
          <common-button size="small" type="success" @click="cancelConfig">取 消</common-button>
          <common-button size="small" type="primary" @click="saveConfig">保 存</common-button>
        </div>
      </el-popover>
      <el-tag hit size="medium" style="margin-left: 5px" effect="plain">
        <span>
          <span>份数：{{ sourcePrintConfig.copiesQuantity }}</span>
        </span>
      </el-tag>
    </template>
    <template #viewLeft>
      <common-button
        v-permission="permission.print"
        type="success"
        size="mini"
        :disabled="crud.selections.length === 0"
        @click="batchPrint(crud.selections)"
        >批量打印标签
      </common-button>
    </template>
  </crudOperation>
</template>

<script setup>
import { materialAdd as addPrintRecord } from '@/api/bridge/label-print/print-record'
import { ref, reactive, inject, defineExpose } from 'vue'

import { rawMatClsEnum } from '@enum-ms/classification'
import { mapGetters } from '@/store/lib'
import { deepClone } from '@data-type/index'
import { spliceQrCodeUrl, QR_SCAN_PATH } from '@/utils/bridge-label'

import usePrintLabel from '@compos/mes/label-print/use-label-print'
import { regHeader } from '@compos/use-crud'
import useGlobalProjectIdChangeToQuery from '@compos/use-global-project-id-change-to-query'
import crudOperation from '@crud/CRUD.operation'
import MaterialCascader from '@comp-cls/material-cascader/index.vue'
import monomerSelect from '@/components-system/plan/monomer-select'
import rrOperation from '@crud/RR.operation'

const defaultQuery = {
  classifyId: undefined,
  monomerId: undefined
}

const { crud, query, CRUD } = regHeader(defaultQuery)
const globalProjectId = useGlobalProjectIdChangeToQuery(crud)
const { requestUrl } = mapGetters(['requestUrl'])

CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data.content.map((v) => {
    v.printedQuantity = v.printQuantity
    v.printQuantity = 1
    return v
  })
}

const printConfigVisible = ref(false)
let printConfig = reactive({
  copiesQuantity: 1
})
const sourcePrintConfig = ref(deepClone(printConfig))

const permission = inject('permission')

const { getLabelInfo, printLabelFunc } = inject('headerObj')
const { batchPrint, print } = usePrintLabel({
  getPrintTotalNumber: (row) => (row.printQuantity || 0) * sourcePrintConfig.value.copiesQuantity,
  getLabelInfo: getLabelInfo,
  printFinallyHook: crud.toQuery,
  getLoadingTextFunc: (row) => `${row.classifyName}-${row.serialNumber}`,
  printLabelFunc: printLabelFunc,
  needAddPrintRecord: true,
  addPrintIdField: 'id',
  addPrintRecordReq: addPrintRecord
})

function handleCopiesChange(val) {
  if (!val) {
    printConfig.copiesQuantity = 1
  }
}

// 取消配置恢复数据
function cancelConfig() {
  printConfigVisible.value = false
  printConfig = Object.assign(printConfig, deepClone(sourcePrintConfig.value))
}
// 保存打印配置
async function saveConfig() {
  sourcePrintConfig.value = deepClone(printConfig)
  printConfigVisible.value = false
}

defineExpose({
  printConfig: sourcePrintConfig,
  spliceQrCodeUrl,
  QR_SCAN_PATH,
  requestUrl,
  print
})
</script>
