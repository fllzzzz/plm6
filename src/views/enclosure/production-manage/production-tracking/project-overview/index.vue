<template>
  <div class="wrap">
    <div class="wrap-left">
      <project-list @project-change="projectChange" />
    </div>
    <div class="wrap-right">
      <div class="app-container">
        <el-tag v-if="!crud.query?.projectId" type="info" effect="plain" size="large"> * 请点击左侧项目列表查看详情 </el-tag>
        <template v-else>
          <!--工具栏-->
          <mHeader ref="headRef" class="head-container overview-head-container" :project="project" @load="load" />
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
                    : `名称：${item.name}\n
                  规格：${item.serialNumber}\n
                  板型：${item.plate}\n
                  品牌：${item.brand}\n
                  颜色：${item.color}\n
                  清单量：${item.quantity}张\n
                  生产量：${item.producedQuantity}张\n
                  排产量：${item.schedulingQuantity}张`
                }`"
                placement="left-start"
              >
                <div class="board-box" :style="{ 'background-color': `${item.boxColor}`, ...boxStyle }">
                  <span class="ellipsis-text">{{ item.name }}</span>
                  <span class="ellipsis-text">{{ item.serialNumber }}</span>
                  <span class="ellipsis-text">{{ item.producedQuantity }}/{{ item.quantity }}</span>
                </div>
              </el-tooltip>
            </template>
            <span v-if="!boardList.length && !crud.loading" class="red-tip">* 暂无数据</span>
            <div v-if="crud.loading" class="loading-box" :style="boxStyle">
              <span>加载中</span>
              <i class="el-icon-loading" />
            </div>
          </div>
        </template>
      </div>
    </div>
  </div>
</template>

<script setup>
import crudApi from '@/api/enclosure/production-manage/project-overview'
import { ref } from 'vue'

import { enclosureProjectOverviewPM as permission } from '@/page-permission/enclosure'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import projectList from './project-list'
import mHeader from './module/header'
import useDashboardIndex from '@compos/mes/dashboard/use-dashboard-index'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const scrollBoxRef = ref()
const headRef = ref()
const tableRef = ref()
const project = ref({})

const { crud, CRUD } = useCRUD(
  {
    title: '项目全貌',
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    requiredQuery: ['projectId', 'planIds'],
    queryOnPresenterCreated: false
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  extraBox: '.overview-head-container',
  paginate: true
})

const { boxStyle, load, boardList } = useDashboardIndex({ headRef, scrollBoxRef, crud, CRUD })

function projectChange(row = {}) {
  project.value = row
  crud.query.projectId = row.id
}
</script>

<style lang="scss" scoped>
.wrap {
  display: flex;

  .wrap-left {
    width: 394px;
  }

  .wrap-right {
    flex: 1;
    min-width: 400px;
    .app-container {
      padding-left: 0;
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
    }
  }
}

::-webkit-scrollbar {
  width: 6px;
  height: 6px;
}
::-webkit-scrollbar-thumb {
  border-radius: 6px;
}
</style>
