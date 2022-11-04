<template>
  <div class="app-container">
    <!--工具栏-->
    <div style="display: flex">
      <div style="width: 70%">
        <mHeader
          ref="headRef"
          @load="load"
          :isIndeterminate="checkedNodes.length > 0 && checkedNodes.length !== boardList && !checkAll"
          @checkedAll="handleCheckedAll"
          @batchMatch="handleBatchMatch"
          @clear="clearCheck"
        />
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
          所需数量：${item.needQuantity}\n
          `"
              placement="left-start"
            >
              <div
                class="board-box"
                style="position: relative; cursor: pointer"
                :style="{ 'background-color': `${item.boxColor}`, ...boxStyle }"
                @click="showStatus(item)"
              >
                <span class="ellipsis-text">{{ item.name }}</span>
                <span class="ellipsis-text">{{ item.serialNumber }}</span>
                <span class="ellipsis-text">{{ item.needQuantity }}</span>
                <el-checkbox
                  style="position: absolute; right: 10px; bottom: 0px"
                  v-model="item.checked"
                  @click.stop
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
      </div>
      <!-- <div style="border-right: 1px solid #ededed; margin: 0 20px; height: calc(100vh - 130px)"></div> -->
      <div style="flex: 1">
        <part-production-list :query="crud.query" />
      </div>
    </div>
    <partProductionStatus v-model:visible="statusVisible" :ids="detailIds" :names="detailNames"></partProductionStatus>
  </div>
</template>

<script setup>
import crudApi from '@/api/mes/production-manage/dashboard/assembly-match'
import { ref } from 'vue'
import { ElMessage } from 'element-plus'

import { DP } from '@/settings/config'
import { assemblyMatchDashboardPM as permission } from '@/page-permission/mes'

import useDashboardIndex from '@compos/mes/dashboard/use-dashboard-index'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import mHeader from './module/header'
import partProductionStatus from './module/part-production-status.vue'
import partProductionList from './module/part-production-list.vue'

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
    title: '项目齐套',
    permission: { ...permission },
    crudApi: { ...crudApi },
    optShow: { ...optShow },
    requiredQuery: ['areaId'],
    queryOnPresenterCreated: false
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: false })

const { boxStyle, load, boardList } = useDashboardIndex({ headRef, scrollBoxRef, crud, CRUD, pageSize: 20, intervalTime: 1000 })

const checkAll = ref(false)
const detailIds = ref([])
const detailNames = ref()
const checkedNodes = ref([])

// 切换项目清除选择
function clearCheck() {
  checkedNodes.value = []
}
function handleCheckedChange(value, item) {
  const _checkedIndex = checkedNodes.value.findIndex((v) => v.id === item.id)
  if (value) {
    if (_checkedIndex === -1) checkedNodes.value.push(item)
  } else {
    if (_checkedIndex > -1) checkedNodes.value.splice(_checkedIndex, 1)
  }
}
function handleCheckedAll(val) {
  checkAll.value = val
  boardList.value.forEach((v) => {
    v.checked = val
    handleCheckedChange(val, v)
  })
}

const statusVisible = ref(false)
function showStatus(item) {
  detailIds.value = [item.id]
  detailNames.value = [{ name: item.serialNumber, tagType: item.tagType }]
  statusVisible.value = true
}

function handleBatchMatch() {
  if (checkedNodes.value.length <= 0) {
    ElMessage.warning('至少选择一个构件进行匹配查询')
    return
  }
  detailIds.value = checkedNodes.value.map((v) => v.id)
  detailNames.value = checkedNodes.value.map((v) => {
    return { name: v.serialNumber, tagType: v.tagType }
  })
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
