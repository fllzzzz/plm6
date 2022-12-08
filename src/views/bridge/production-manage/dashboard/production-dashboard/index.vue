<template>
  <div class="app-container">
    <!--工具栏-->
    <mHeader ref="headRef" @load="load" />
    <div style="display: flex" class="artifact-dashboard">
      <common-table
        v-if="crud.query.productType & componentTypeEnum.MACHINE_PART.V"
        :data="specList"
        highlight-current-row
        :stripe="false"
        ref="tableRef"
        style="width: 60px; margin-right: 10px"
        @row-click="handleRowClick"
      >
        <el-table-column header-align="center" align="center" prop="name" label="板厚" min-width="50">
          <template #default="{ row }">
            <span style="cursor: default">{{ row.name }}</span>
          </template>
        </el-table-column>
      </common-table>
      <!--看板渲染-->
      <div
        v-if="crud.firstLoaded"
        ref="scrollBoxRef"
        v-infinite-scroll="load"
        class="board-container"
        :infinite-scroll-disabled="crud.loading || !crud.page.hasNextPage"
        :infinite-scroll-delay="200"
        :infinite-scroll-distance="200"
        :infinite-scroll-immediate-check="true"
        style="flex: 1"
        :style="{ 'max-height': `${maxHeight}px` }"
      >
        <template v-for="item in boardList" :key="item.id">
          <el-tooltip
            :show-after="300"
            class="item"
            effect="light"
            :content="`${item.detailLoading ? '正在加载中...' : `${item.processInfo}`}`"
            placement="left-start"
          >
            <div class="board-box" :style="{ 'background-color': `${item.boxColor}`, ...boxStyle }" @mouseenter="getDetail(item)">
              <span class="ellipsis-text" v-if="item.name">{{ item.name }}</span>
              <span class="ellipsis-text">{{ item.serialNumber }}</span>
              <span class="ellipsis-text">{{ item.completeQuantity }}/{{ item.compareQuantity }}</span>
            </div>
          </el-tooltip>
        </template>
        <span v-if="!boardList.length && !crud.loading" class="red-tip">* 暂无数据</span>
        <div v-if="crud.loading" class="loading-box" :style="boxStyle">
          <span>加载中</span>
          <i class="el-icon-loading" />
        </div>
      </div>
    </div>
  </div>
</template>

<script setup>
import { productDashboard as get, productSpec } from '@/api/bridge/bridge-production-manage/common'
import { artifactDetail, assembleDetail, baseAssembleDetail, machinePartDetail } from '@/api/bridge/bridge-production-manage/dashboard'
import { ref } from 'vue'

import { structureOrderTypeEnum } from '@enum-ms/mes'
import { componentTypeEnum } from '@enum-ms/bridge'
import { DP } from '@/settings/config'
import { bridgeArtifactProductionDashboardPM as permission } from '@/page-permission/bridge'

import useDashboardIndex from '@compos/mes/dashboard/use-dashboard-index'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import mHeader from './module/header'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const scrollBoxRef = ref()
const headRef = ref()
const tableRef = ref()
const { crud, CRUD } = useCRUD(
  {
    title: '构件看板',
    permission: { ...permission },
    crudApi: { get },
    optShow: { ...optShow },
    requiredQuery: ['areaId'],
    queryOnPresenterCreated: false
  },
  tableRef
)
const { maxHeight } = useMaxHeight({ paginate: false })

const { boxStyle, load, boardList } = useDashboardIndex({ headRef, scrollBoxRef, crud, CRUD, beforeRefreshHook })

async function getDetail(item) {
  switch (crud.query.productType) {
    case componentTypeEnum.BOX.V:
      getArtifactDetail(item)
      break
    case componentTypeEnum.CELL.V:
      getAssembleDetail(item)
      break
    case componentTypeEnum.MACHINE_PART.V:
      getMachinePartDetail(item)
      break
    default:
      break
  }
}

const specList = ref([])
const specLoading = ref(false)
const isSpecQuery = ref(false)
const curSpec = ref({})

function handleRowClick(row) {
  if (curSpec.value?.name && row.name === curSpec.value?.name) {
    tableRef.value.setCurrentRow()
    curSpec.value = {}
    crud.query.steelSpec = undefined
  } else {
    tableRef.value.setCurrentRow(row)
    curSpec.value = row
    crud.query.steelSpec = row.name
  }
  isSpecQuery.value = true
  crud.toQuery()
}

async function fetchSpec() {
  try {
    specLoading.value = true
    const { content } = await productSpec(crud.query)
    specList.value = content
  } catch (error) {
    console.log('获取规格', error)
  }
}

function beforeRefreshHook() {
  if (crud.query.productType & componentTypeEnum.MACHINE_PART.V && !isSpecQuery.value) {
    fetchSpec()
  }
  isSpecQuery.value = false
}

async function getArtifactDetail(item) {
  if (item.hasDetail) return
  try {
    item.detailLoading = true
    const _data = await artifactDetail({ id: item.id })
    item.hasDetail = true
    _data.processInfo = `${_data.name} ${_data.serialNumber}\n
          规格：${_data.specification}\n
          长度：${_data.length} mm\n
          材质：${_data.material}\n
          单净重：${_data.netWeight && _data.netWeight.toFixed(DP.COM_WT__KG)} kg\n
          单毛重：${_data.grossWeight && _data.grossWeight.toFixed(DP.COM_WT__KG)} kg\n
          图号：${_data.drawingNumber}\n
          清单数量：${_data.quantity}\n`
    _data.processInfo += '-----------------------\n\n生产上报 / 已质检\n\n'
    const processList = _data.processSummaryDetailsList || []
    processList.forEach((process) => {
      const _completed = _data.quantity === process.completeQuantity && process.quantity === process.inspectionQuantity
      const _processInfo = _completed ? `√` : `${process.completeQuantity} / ${process.inspectionQuantity}`
      _data.processInfo += `${process.name}：${_processInfo}\n\n`
    })
    item = Object.assign(item, { processInfo: _data.processInfo })
  } catch (error) {
    console.log('获取详情失败', error)
  } finally {
    item.detailLoading = false
  }
}

async function getAssembleDetail(item) {
  if (item.hasDetail) return
  try {
    item.detailLoading = true
    const _data =
      item.productType === structureOrderTypeEnum.NESTING.V ? await baseAssembleDetail(item.id) : await assembleDetail({ id: item.id })
    item.hasDetail = true
    if (item.productType === structureOrderTypeEnum.NESTING.V) {
      _data.processInfo = `${_data.serialNumber}\n
          长度（mm）：${_data.length}\n
          材质：${_data.material}\n
          规格：${_data.specification}\n`
      _data.processInfo += '-----------------------\n\n编号       数量\n\n'
      const processList = _data.assembleLinkDTOS || []
      processList.forEach((process) => {
        const _processInfo = process.quantity
        _data.processInfo += `${process.serialNumber}：${_processInfo}\n\n`
      })
    } else {
      _data.processInfo = `${_data.serialNumber}\n
          清单数量：${_data.quantity}\n
          已生产数量：${_data.producedQuantity}\n
          已使用数量：${_data.usedQuantity}\n`
      _data.processInfo += '-----------------------\n\n生产上报 / 已质检\n\n'
      const processList = _data.processSummaryDetailsList || []
      processList.forEach((process) => {
        const _completed = _data.quantity === process.completeQuantity && process.quantity === process.inspectionQuantity
        const _processInfo = _completed ? `√` : `${process.completeQuantity} / ${process.inspectionQuantity}`
        _data.processInfo += `${process.name}：${_processInfo}\n\n`
      })
    }

    item = Object.assign(item, { processInfo: _data.processInfo })
  } catch (error) {
    console.log('获取详情失败', error)
  } finally {
    item.detailLoading = false
  }
}

async function getMachinePartDetail(item) {
  if (item.hasDetail) return
  try {
    item.detailLoading = true
    const _data = await machinePartDetail({ id: item.id })
    item.hasDetail = true
    _data.processInfo = `${_data.serialNumber}\n
          清单数量：${_data.quantity}\n
          已生产数量：${_data.producedQuantity}\n
          已使用数量：${_data.usedQuantity}\n`
    _data.processInfo += '-----------------------\n\n生产上报 / 已质检\n\n'
    const processList = _data.processSummaryDetailsList || []
    processList.forEach((process) => {
      const _completed = _data.quantity === process.completeQuantity && process.quantity === process.inspectionQuantity
      const _processInfo = _completed ? `√` : `${process.completeQuantity} / ${process.inspectionQuantity}`
      _data.processInfo += `${process.name}：${_processInfo}\n\n`
    })
    item = Object.assign(item, { processInfo: _data.processInfo })
  } catch (error) {
    console.log('获取详情失败', error)
  } finally {
    item.detailLoading = false
  }
}
</script>

<style lang="scss" scoped>
.board-container {
  display: flex;
  flex-direction: row;
  justify-content: flex-start;
  // align-items: center;
  flex-wrap: wrap;
  overflow: auto;

  .board-box {
    width: 120px;
    height: 120px;
    box-sizing: border-box;
    padding: 2px;
    margin: 0 10px 10px 0;
    font-size: 16px;
    display: flex;
    flex-direction: column;
    justify-content: space-around;
    align-items: center;
    border: 1px solid #dfe4ed;
    border-radius: 6px;

    span {
      display: inline-block;
      width: 100%;
      text-align: center;
    }
  }
  .loading-box {
    width: 120px;
    height: 120px;
    display: flex;
    flex-direction: column;
    justify-content: center;
    align-items: center;
    color: #46a6ff;
    > span {
      margin-bottom: 10px;
    }
  }
  .gif-content {
    width: 260px;
    height: 120px;
  }
}
</style>

<style lang="scss">
.artifact-dashboard .el-table__body tr:hover > td {
  background-color: transparent !important;
}
.artifact-dashboard .el-table__body tr.current-row > td {
  background-color: #999 !important;
  color: #fff;
}
</style>
