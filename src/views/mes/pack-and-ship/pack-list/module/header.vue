<template>
  <div v-show="crud.searchToggle">
    <project-radio-button size="small" v-model="query.projectId" class="filter-item" @change="crud.toQuery" />
    <common-radio-button
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
        <el-form ref="form" :model="printConfig" label-width="90px">
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
      <el-tag v-permission="permission.print" hit effect="plain" size="medium" style="margin-left: 5px">{{ `份数：${printConfig.copies}` }}</el-tag>
      <!-- <print-table
        v-permission="permission.printPackList"
        api-key="STEEL_MES_PACK"
        :params="printParams"
        :before-print="handleBeforePrint"
        size="mini"
        type="warning"
        class="filter-item"
      /> -->
    </template>
    <template #viewLeft>
      <common-button
v-permission="permission.print"
type="success"
size="mini"
:disabled="crud.selections.length === 0"
@click="batchPrint"
        >批量打印标签</common-button
      >
    </template>
  </crudOperation>
</template>

<script setup>
import { packageRecordAdd } from '@/api/mes/label-print/print-record'
import { inject, reactive, ref, defineExpose } from 'vue'
import { mapGetters } from '@/store/lib'
import { ElNotification, ElLoading } from 'element-plus'
import moment from 'moment'

import { packTypeEnum, qrCodeTypeEnum } from '@enum-ms/mes'
import { PICKER_OPTIONS_SHORTCUTS } from '@/settings/config'
import { printPackageLabel } from '@/utils/print/index'
import { DP } from '@/settings/config'
// import { isNotBlank } from '@data-type/index'
import { parseTime } from '@/utils/date'
import { codeWait } from '@/utils'

import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'

const permission = inject('permission')
const { companyName } = mapGetters(['companyName'])
const defaultQuery = {
  serialNumber: undefined,
  userName: undefined,
  startDate: undefined,
  endDate: undefined,
  remark: void 0,
  materialTypeArr: { value: undefined, resetAble: false },
  productType: packTypeEnum.STRUCTURE.V,
  projectId: { value: undefined, resetAble: false }
}
const { crud, query } = regHeader(defaultQuery)

const printLoading = ref()
const printConfig = reactive({
  manufacturerName: companyName,
  copies: 1
})

// const printParams = computed(() => {
//   if (isNotBlank(crud.selections)) {
//     return crud.selections.map((row) => {
//       return row.id
//     })
//   }
//   return undefined
// })

// function handleBeforePrint() {
//   if (!isNotBlank(printParams)) {
//     ElMessage.warning('至少选择一条需要打印的包单信息')
//     return false
//   }
// }

async function batchPrint() {
  try {
    await print(crud.selections)
    crud.selectAllChange()
  } catch (error) {
    console.log('批量打印错误', error)
  }
}

async function print(rows) {
  printLoading.value = ElLoading.service({
    target: '#printContainer',
    lock: true,
    text: '正在准备加入打印队列',
    spinner: 'el-icon-loading',
    fullscreen: false
  })
  try {
    for (const row of rows) {
      await printLabel(row)
    }
    printLoading.value.text = `已全部加入打印队列`
    await codeWait(500)
  } catch (error) {
    ElNotification({ title: '加入打印队列失败，请重试', type: 'error', duration: 2500 })
    throw new Error(error)
  } finally {
    printLoading.value.close()
    crud.toQuery()
  }
}

async function printLabel(row) {
  let pollingTimes = printConfig.copies // 打印总次数
  let printedTimes = 0 // 已打印次数
  const startTime = new Date().getTime()
  try {
    const labelInfo = getlabelInfo(row)
    while (pollingTimes--) {
      printLoading.value.text = `正在加入打印队列：${row.serialNumber} 第${printedTimes + 1}张`
      await printPackageLabel(labelInfo)
      printedTimes++
    }
  } catch (error) {
    console.log('打印标签时发生错误', error)
    throw new Error(error)
  } finally {
    const endTime = new Date().getTime()
    addPrintRecord(row, { id: row.id, quantity: printedTimes, startTime, endTime })
  }
}

async function addPrintRecord(row, { id, quantity, startTime, endTime }) {
  if (!id || !quantity) return
  try {
    await packageRecordAdd({ id, quantity, startTime, endTime })
    row.printedQuantity += quantity
  } catch (error) {
    console.log('添加打印记录失败', error)
  }
}

function getlabelInfo(row) {
  // 标签构件信息
  const packageInfo = {
    projectName: row.project.name,
    serialNumber: row.serialNumber,
    totalWeight: row.totalGrossWeight && row.totalGrossWeight.toFixed(DP.COM_WT__KG),
    quantity: row.quantity,
    materialTypeNames: row.materialTypeNames,
    createTime: parseTime(row.createTime, '{y}-{m}-{d}'),
    packerName: row.packerName,
    remark: row.remark
  }
  // 生产线信息
  return {
    packageInfo,
    qrCode: JSON.stringify({
      id: row.id,
      type: qrCodeTypeEnum.PACKAGE.V
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
  getlabelInfo
})
</script>
