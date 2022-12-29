<template>
  <div class="app-container">
    <div v-if="globalProject?.businessType===businessTypeEnum.INSTALLATION.V">
      <!--工具栏-->
      <mHeader ref="headRef" @load="load" :globalProject="globalProject"/>
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
            :content="`${globalProject.projectType === projectTypeEnum.STEEL.V?item.name:''}${item.serialNumber}\n
            收货数量：${item.receivingQuantity}\n
            清单数量：${item.quantity}\n`"
            placement="left-start"
          >
            <div class="board-box" :style="{ 'background-color': `${item.boxColor}`, ...boxStyle }">
              <span class="ellipsis-text" v-if="globalProject.projectType === projectTypeEnum.STEEL.V">{{ item.name }}</span>
              <span class="ellipsis-text">{{ item.serialNumber }}</span>
              <span class="ellipsis-text">{{ item.receivingQuantity }}/{{ item.quantity }}</span>
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
    <div v-else>
      <el-tag type="danger" size="medium" style="margin-bottom: 10px"> * 您好，请先选择业务类型为项目承包的项目，当前页面需要选择业务类型为项目承包方可查看 </el-tag>
    </div>
  </div>
</template>

<script setup>
import { deliveryDashboardData as get } from '@/api/project-manage/delivery-manage/delivery-report/report-list'
import { ref, watch } from 'vue'

import { projectTypeEnum } from '@enum-ms/contract'
import { businessTypeEnum } from '@enum-ms/contract'
import { deliveryReportDashboardPM as permission } from '@/page-permission/project'
import { mapGetters } from '@/store/lib'

import useDashboardIndex from '@compos/mes/dashboard/use-dashboard-index'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import mHeader from './module/header'

const { globalProjectId, globalProject } = mapGetters(['globalProjectId', 'globalProject'])

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
    title: '收货看板',
    permission: { ...permission },
    crudApi: { get },
    optShow: { ...optShow },
    requiredQuery: ['projectId', 'productType'],
    queryOnPresenterCreated: false
  },
  tableRef
)
const { maxHeight } = useMaxHeight({ paginate: false })

const { boxStyle, load, boardList } = useDashboardIndex({ headRef, scrollBoxRef, crud, CRUD })

watch(
  () => globalProjectId.value,
  (val) => {
    if (val) {
      crud.query.projectId = globalProjectId.value
      crud.toQuery()
    }
  },
  { immediate: true, deep: true }
)
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
