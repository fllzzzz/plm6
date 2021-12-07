<template>
  <div class="app-container">
    <!--工具栏-->
    <mHeader ref="headRef" @load="load" :isIndeterminate="checkedIds.length > 0 && checkedIds.length !== boardList" @checkedAll="handleCheckedAll"/>
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
          清单数量：${item.quantity}\n
          `"
          placement="left-start"
        >
          <div class="board-box" style="position: relative" :style="{ 'background-color': `${item.boxColor}`, ...boxStyle }" @click="showStatus">
            <span class="ellipsis-text">{{ item.name }}</span>
            <span class="ellipsis-text">{{ item.serialNumber }}</span>
            <span class="ellipsis-text">{{ item.quantity }}</span>
            <el-checkbox
              style="position: absolute; right: 10px; bottom: 0px"
              v-model="item.checked"
              @change="handleCheckedChange($event, item)"
            ></el-checkbox>
          </div>
        </el-tooltip>
      </template>
      <span v-if="!boardList.length && !crud.loading" class="red-tip">* 暂无数据</span>
      <div v-if="crud.loading" class="loading-box" :style="boxStyle">
        <span>加载中</span>
        <i class="el-icon-loading" />
      </div>
    </div>
    <partProductionStatus v-model:visible="statusVisible"></partProductionStatus>
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
import partProductionStatus from './module/part-production-status.vue'

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
    title: '总装匹配',
    permission: { ...permission },
    crudApi: { get },
    optShow: { ...optShow }
    // requiredQuery: ['districtId'],
    // queryOnPresenterCreated: false
  },
  tableRef
)
const { maxHeight } = useMaxHeight({ paginate: false })

const { boxStyle, load, boardList } = useDashboardIndex({ headRef, scrollBoxRef, crud, CRUD })

const checkedIds = ref([])
function handleCheckedChange(value, item) {
  const _checkedIndex = checkedIds.value.indexOf(item.id)
  if (value) {
    if (_checkedIndex === -1) checkedIds.value.push(item.id)
  } else {
    if (_checkedIndex > -1) checkedIds.value.splice(_checkedIndex, 1)
  }
}
function handleCheckedAll(val) {
  boardList.value.forEach((v) => {
    v.checked = val
    handleCheckedChange(val, v)
  })
}

const statusVisible = ref(false)
function showStatus() {
  statusVisible.value = true
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
