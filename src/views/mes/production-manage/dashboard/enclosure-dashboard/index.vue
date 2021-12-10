<template>
  <div class="app-container">
    <!--工具栏-->
    <mHeader ref="headRef" @load="load" />
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
      :style="{ 'max-height': `${maxHeight}px` }"
    >
      <template v-for="item in boardList" :key="item.id">
        <el-tooltip
          :open-delay="300"
          class="item"
          effect="light"
          :content="`${
            item.detailLoading
              ? '正在加载中...'
              : `${item.name} ${item.serialNumber}\n
          板型：${item.plate}\n
          颜色：${item.color}\n
          厚度：${item.thickness && item.thickness.toFixed(DP.MES_ENCLOSURE_T__MM)} mm\n
          长度：${item.length && item.length.toFixed(DP.MES_ENCLOSURE_L__MM)} mm\n
          有效宽度：${item.width && item.width.toFixed(DP.MES_ENCLOSURE_W__MM)} mm\n
          清单数量：${item.quantity}\n
          ${item.processInfo}`
          }`"
          placement="left-start"
        >
          <div class="board-box" :style="{ 'background-color': `${item.boxColor}`, ...boxStyle }" @mouseenter="getDetail(item)">
            <span class="ellipsis-text">{{ item.name }}</span>
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
</template>

<script setup>
import { productDashboard as get } from '@/api/mes/production-manage/dashboard/common'
import { detail } from '@/api/mes/production-manage/dashboard/enclosure'
import { ref } from 'vue'

import { DP } from '@/settings/config'

import useDashboardIndex from '@compos/mes/dashboard/use-dashboard-index'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import mHeader from './module/header'

const permission = {
  get: ['enclosureInboundStateDashboard:get']
}

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
    title: '围护看板',
    permission: { ...permission },
    crudApi: { get },
    optShow: { ...optShow }
    // requiredQuery: ['areaId'],
    // queryOnPresenterCreated: false
  },
  tableRef
)
const { maxHeight } = useMaxHeight({ paginate: false })

const { boxStyle, load, boardList } = useDashboardIndex({ headRef, scrollBoxRef, crud, CRUD })

async function getDetail(item) {
  if (item.hasDetail) return
  try {
    item.detailLoading = true
    const _data = await detail({ id: item.id })
    item.hasDetail = true
    _data.processInfo = '-----------------------\n\n生产上报 / 已质检\n\n'
    const processList = _data.processSummaryDetailsList || []
    processList.forEach((process) => {
      const _completed = _data.quantity === process.completeQuantity && process.quantity === process.inspectionQuantity
      const _processInfo = _completed ? `√` : `${process.completeQuantity} / ${process.inspectionQuantity}`
      _data.processInfo += `${process.name}：${_processInfo}\n\n`
    })
    item = Object.assign(item, _data)
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
  align-items: center;
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
