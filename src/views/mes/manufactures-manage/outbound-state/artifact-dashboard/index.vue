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
          :content="`${item.name} ${item.serialNumber}\n
          规格：${item.specification}\n
          长度：${item.length} mm\n
          材质：${item.material}\n
          单净重：${item.netWeight.toFixed(DP.COM_WT__KG)} kg\n
          单毛重：${item.grossWeight.toFixed(DP.COM_WT__KG)} kg\n
          图号：${item.drawingNumber}\n
          ${item.quantityInfo}\n`"
          placement="left-start"
        >
          <div class="board-box" :style="{ 'background-color': `${item.boxColor}`, ...boxStyle }">
            <span class="ellipsis-text">{{ item.name }}</span>
            <span class="ellipsis-text">{{ item.serialNumber }}</span>
            <span class="ellipsis-text">{{ item.outboundQuantity }}/{{ item.compareQuantity }}</span>
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
import { getBoardForArtifact as get } from '@/api/mes/manufactures-manage/common'
import { ref } from 'vue'

import { DP } from '@/settings/config'

import useDashboardIndex from '@compos/mes/dashboard/use-dashboard-index'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import mHeader from './module/header'

const permission = {
  get: ['artifactInboundStateDashboard:get']
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

const { boxStyle, load, boardList } = useDashboardIndex({ headRef, scrollBoxRef, crud, CRUD })
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
