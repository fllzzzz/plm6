<template>
  <div v-show="crud.searchToggle">
    <project-radio-button size="small" v-model="query.projectId" class="filter-item" @change="crud.toQuery" />
    <component-radio-button
      v-model="query.productType"
      :options="packTypeEnum.ENUM"
      type="enum"
      size="small"
      class="filter-item"
      @change="crud.toQuery"
    />
    <el-date-picker
      v-model="query.date"
      type="daterange"
      unlink-panels
      range-separator="至"
      start-placeholder="开始日期"
      end-placeholder="结束日期"
      size="small"
      class="filter-item"
      style="width: 400px"
      clearable
      :shortcuts="PICKER_OPTIONS_SHORTCUTS"
      @change="handleDateChange"
    />
    <el-input
      v-model="query.serialNumber"
      placeholder="输入包单搜索"
      class="filter-item"
      style="width: 200px"
      size="small"
      clearable
      @keyup.enter="crud.toQuery"
    />
    <el-input
      v-model="query.userName"
      placeholder="输入打包人名称搜索"
      class="filter-item"
      style="width: 200px"
      size="small"
      clearable
      @keyup.enter="crud.toQuery"
    />
    <el-input
      v-model="query.remark"
      placeholder="输入备注搜索"
      class="filter-item"
      style="width: 200px"
      size="small"
      clearable
      @keyup.enter="crud.toQuery"
    />
    <rrOperation />
  </div>
  <crudOperation>
    <template #optRight>
      <el-popover placement="bottom-start" width="400" trigger="click">
        <el-form ref="form" :model="printConfig">
          <!-- <el-form-item label="制造商名称">
              <el-input v-model="printConfig.manufacturerName" size="small" style="width:250px" />
            </el-form-item> -->
          <el-form-item label="份数">
            <el-input-number
              v-model="printConfig.copies"
              :step="1"
              :min="1"
              size="small"
              style="width: 250px"
              @change="handleCopiesChange"
            />
          </el-form-item>
        </el-form>
        <template #reference>
          <common-button type="primary" size="mini">标签打印设置</common-button>
        </template>
      </el-popover>
      <el-tag v-permission="permission.print" hit effect="plain" size="medium" style="margin-left: 5px">{{
        `份数：${printConfig.copies}`
      }}</el-tag>
      <print-table
        v-permission="permission.printPackList"
        api-key="bridgePackingList"
        :params="printParams"
        :before-print="handleBeforePrint"
        size="mini"
        type="warning"
        class="filter-item"
        style="margin-left: 5px !important"
      />
    </template>
    <template #viewLeft>
      <common-button
        v-permission="permission.print"
        type="success"
        size="mini"
        :disabled="crud.selections.length === 0"
        @click="batchPrint(crud.selections)"
        >批量打印标签</common-button
      >
    </template>
  </crudOperation>
</template>

<script setup>
import { detail } from '@/api/bridge/bridge-pack-and-ship/pack-list'
import { packageRecordAdd } from '@/api/bridge/label-print/print-record'
import { inject, reactive, defineExpose, computed, defineEmits } from 'vue'
import { mapGetters } from '@/store/lib'
import moment from 'moment'

import { bridgePackTypeEnum as packTypeEnum } from '@enum-ms/bridge'
import { PICKER_OPTIONS_SHORTCUTS } from '@/settings/config'
import { printPackageLabel } from '@/utils/print/index'
import { QR_SCAN_F_TYPE, QR_SCAN_TYPE } from '@/settings/config'
import { DP } from '@/settings/config'
import { isNotBlank } from '@data-type/index'

import usePrintLabel from '@compos/bridge/label-print/use-label-print'
import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'
import { ElMessage } from 'element-plus'

const emit = defineEmits(['getDetail'])
const permission = inject('permission')
const { user } = mapGetters(['user'])
const defaultQuery = {
  serialNumber: undefined,
  userName: undefined,
  startDate: undefined,
  endDate: undefined,
  remark: void 0,
  materialTypeArr: { value: undefined, resetAble: false },
  productType: packTypeEnum.BOX.V,
  projectId: { value: undefined, resetAble: false }
}
const { crud, query } = regHeader(defaultQuery)

const printConfig = reactive({
  manufacturerName: user.value.companyName,
  copies: 1
})

const printParams = computed(() => {
  if (isNotBlank(crud.selections)) {
    return crud.selections.map((row) => {
      return row.id
    })
  }
  return undefined
})

function handleBeforePrint() {
  if (!isNotBlank(printParams.value)) {
    ElMessage.warning('至少选择一条需要打印的包单信息')
    return false
  }
}

const { batchPrint, print } = usePrintLabel({
  getPrintTotalNumber: () => computed(() => printConfig.copies).value,
  getLabelInfo: getLabelInfo,
  printFinallyHook: crud.toQuery,
  getLoadingTextFunc: (row) => `${row.serialNumber}`,
  printLabelFunc: printPackageLabel,
  needAddPrintRecord: true,
  addPrintRecordReq: packageRecordAdd
})

const detailStore = inject('detailStore')
const dataField = {
  [packTypeEnum.BOX.V]: 'artifactList',
  [packTypeEnum.CELL.V]: 'enclosureList',
  [packTypeEnum.AUXILIARY_MATERIAL.V]: 'materialList'
}

async function getLabelInfo(row) {
  let _list = []
  let _data = {}
  try {
    if (detailStore[row.id]) {
      _data = detailStore[row.id]
    } else {
      _data = await detail(row.id)
      emit('getDetail', row.id, _data)
    }
    _list = _data[dataField[row.productType]].map((v) => {
      const { serialNumber, material, packageQuantity, grossWeight, plate, length } = v
      return {
        serialNumber,
        material,
        quantity: packageQuantity,
        totalWeight: (packageQuantity * grossWeight).toFixed(DP.COM_WT__KG),
        // totalNetWeight: totalNetWeight ? totalNetWeight.toFixed(DP.COM_WT__KG) : 0,
        plate,
        length: length ? length.toFixed(DP.MES_ENCLOSURE_L__MM) : 0
      }
    })
  } catch (error) {
    console.log('获取详情失败', error)
  }

  // 标签分段信息
  const packageInfo = {
    serialNumber: row.serialNumber,
    list: _list,
    productType: row.productType,
    project: row.project,
    companyName: printConfig.manufacturerName
  }
  // 生产线信息
  return {
    packageInfo,
    qrCode: JSON.stringify({
      id: row.id,
      type: QR_SCAN_TYPE.MES_PACKAGE,
      ftype: QR_SCAN_F_TYPE.MES_PACKAGE_SHIP
    })
  }
}

function handleDateChange() {
  if (query.date && query.date.length > 1) {
    query.startDate = moment(query.date[0]).valueOf()
    query.endDate = moment(query.date[1]).valueOf()
  } else {
    query.startDate = undefined
    query.endDate = undefined
  }
  crud.toQuery()
}

function handleCopiesChange(val) {
  if (!val) {
    printConfig.copies = 1
  }
}

defineExpose({
  print,
  getLabelInfo
})
</script>
