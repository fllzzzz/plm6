<template>
  <div class="app-container wrap">
    <div class="wrap-left">
      <project-list ref="projectListRef" :maxHeight="maxHeight" @project-click="handleProjectClick" />
    </div>
    <div class="wrap-right">
      <el-tag v-show="!crud.query?.projectIds?.length" type="info" size="medium"> * 请先选择项目，进行零件排产 </el-tag>
      <div v-show="crud.query?.projectIds?.length">
        <div class="head-container">
          <mHeader ref="headRef" @load="load">
            <template #optLeft>
              <div style="display: flex">
                <el-checkbox
                  v-model="checkAll"
                  size="mini"
                  style="margin-right: 5px"
                  :indeterminate="checkedNodes.length > 0 && checkedNodes.length !== boardList && !checkAll"
                  border
                  @change="handleCheckedAll"
                  >全选</el-checkbox
                >
                <common-button
v-permission="permission.save"
type="success"
class="filter-item"
size="mini"
@click="previewIt"
                  >预览并保存</common-button
                >
              </div>
            </template>
            <template #viewLeft>
              <el-tag size="medium" effect="plain" style="margin-right: 5px"> 数量(件)：{{ summaryInfo.quantity || 0 }} </el-tag>
              <el-tag size="medium" effect="plain" style="margin-right: 10px">
                重量(kg)：{{ summaryInfo.totalNetWeight?.toFixed(2) || 0 }}
              </el-tag>
            </template>
          </mHeader>
        </div>
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
          :style="{ 'max-height': `${maxHeight - 90}px` }"
        >
          <template v-for="item in boardList" :key="item.id">
            <el-tooltip
              v-model="item.visibleTip"
              manual
              :open-delay="300"
              class="item"
              effect="light"
              :content="`${item.project?.shortName}\n
          编号：${item.serialNumber}\n
          长度：${item.length} mm\n
          单重：${item.netWeight} kg\n
          数量：${item.quantity}\n
          `"
              placement="left-start"
            >
              <div
                class="board-box"
                style="position: relative; cursor: pointer"
                :style="{ 'background-color': `${item.boxColor}`, ...boxStyle }"
                @mouseleave="item.visibleTip = false"
              >
                <div style="display: flex; justify-content: space-between; width: 100%; align-items: center; padding: 0 5px">
                  <el-checkbox
                    v-model="item.checked"
                    :disabled="!item.imgLoad && crud.query.thick !== '其他'"
                    @click.stop
                    @change="handleCheckedChange($event, item)"
                  ></el-checkbox>
                  <span class="ellipsis-text text">
                    {{ item.serialNumber }}
                  </span>
                </div>
                <el-image style="flex: 1; width: 95%" :src="item.picturePath" @error="item.imgLoad = false">
                  <template #error>
                    <div class="error-slot">
                      <span v-if="item.picturePath">加载失败</span>
                      <span v-else>未导入DXF</span>
                    </div>
                  </template>
                </el-image>
                <span
class="ellipsis-text text"
@click.stop="item.visibleTip = !item.visibleTip"
                  >{{ item.specification }}/{{ item.quantity }}</span
                >
              </div>
            </el-tooltip>
          </template>
          <span v-if="!boardList.length && !crud.loading" class="red-tip">* 暂无数据</span>
          <div v-if="crud.loading" class="loading-box" :style="boxStyle">
            <span>加载中</span>
            <i class="el-icon-loading" />
          </div>
        </div>
        <m-preview v-model:visible="previewVisible" :list="checkedNodes" @success="handleSaveSuccess"></m-preview>
      </div>
    </div>
  </div>
</template>

<script setup>
import crudApi from '@/api/bridge/scheduling-manage/machine-part'
import { computed, ref, onUnmounted, onMounted, nextTick } from 'vue'
import { ElMessage } from 'element-plus'

import RAF from '@/utils/raf'
// import { bridgeComponentTypeEnum } from '@enum-ms/bridge'
import { machinePartSchedulingPM as permission } from '@/page-permission/bridge'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import mHeader from './module/header'
import mPreview from './module/preview'
import projectList from './module/project-list'
import { deepClone } from '@/utils/data-type'
import { cleanArray } from '@data-type/array'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const projectListRef = ref()
const tableRef = ref()
const { crud, CRUD } = useCRUD(
  {
    title: '零件排产',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    queryOnPresenterCreated: false,
    hasPagination: false,
    requiredQuery: ['monthList', 'material', 'projectIds', 'thick']
  },
  tableRef
)

const { maxHeight } = useMaxHeight()

const boardList = ref([])
const summaryInfo = ref({ totalNetWeight: 0, quantity: 0 })

CRUD.HOOK.handleRefresh = (crud, res) => {
  clearCheck()
  summaryInfo.value.totalNetWeight = res.data?.totalNetWeight || 0
  summaryInfo.value.quantity = res.data?.quantity || 0
  res.data.content = res.data.collect.map((v) => {
    v.checked = false
    v.visibleTip = false
    v.imgLoad = true
    return v
  })
}

// --------------------------- start ------------------------------
const headRef = ref()
const scrollBoxRef = ref()
const intervalTime = 1000

const boxScale = computed(() => {
  if (headRef.value) {
    checkHasScrollBar()
    return headRef.value.boxScale
  }
  return 1
})

const boxStyle = computed(() => {
  return {
    'font-size': `${(16 * boxScale.value).toFixed(0)}px`,
    width: `${(120 * boxScale.value).toFixed(0)}px`,
    height: `${(120 * boxScale.value).toFixed(0)}px`
  }
})

onMounted(() => {
  // 处理容器一开始撑满，size改变之后，未撑满的情况
  window.addEventListener('resize', checkHasScrollBar, { passive: false })
})

onUnmounted(() => {
  window.removeEventListener('resize', checkHasScrollBar)
})

function checkHasScrollBar() {
  RAF.clearInterval()
  const distance = 200
  const boxEl = scrollBoxRef.value
  const flag = !boxEl || !crud.page.hasNextPage || boxEl.scrollHeight > boxEl.clientHeight + distance
  if (flag) return
  let pollingTimes = 0 // 避免异常无限轮询
  console.log(intervalTime)
  RAF.setInterval(() => {
    const _flag = boxEl && crud.page.hasNextPage && boxEl.scrollHeight < boxEl.clientHeight + distance
    if (_flag && ++pollingTimes <= 10) {
      load()
    } else {
      RAF.clearInterval()
    }
  }, intervalTime)
}

async function load() {
  if (crud.firstLoaded && crud.page.hasNextPage) {
    await crud.pageChangeHandler(++crud.page.page)
  }
}

CRUD.HOOK.beforeRefresh = () => {
  boardList.value = []
  summaryInfo.value = { totalNetWeight: 0, quantity: 0 }
}

CRUD.HOOK.afterRefresh = () => {
  crud.data.forEach((component) => {
    boardList.value.push(component)
  })
}

// --------------------------- end --------------------------------

function handleProjectClick(val, month) {
  crud.query.monthList = month
  crud.query.projectIds = cleanArray(val).map((v) => v.projectId)
  nextTick(() => {
    headRef.value?.refreshConditions()
  })
}

async function handleSaveSuccess() {
  console.log(crud.query.projectIds)
  const lastQuery = deepClone(crud.query)
  checkAll.value = false
  boardList.value = []
  crud.page.page = 1
  await projectListRef?.value?.refresh(lastQuery)
  await headRef.value?.refreshConditions(lastQuery)
}

// --------------------------- 选择操作 start ------------------------------

const checkAll = ref(false)
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
    if (v.imgLoad || crud.query.thick === '其他') {
      v.checked = val
      handleCheckedChange(val, v)
    }
  })
}

// --------------------------- 选择操作 end --------------------------------

// --------------------------- 预览并保存 start ------------------------------

const previewVisible = ref(false)

function previewIt() {
  if (!checkedNodes.value?.length) {
    ElMessage.warning('请至少选择一条数据')
    return
  }

  previewVisible.value = true
}
// --------------------------- 预览并保存 end --------------------------------
</script>

<style lang="scss" scoped>
.error-slot {
  display: flex;
  justify-content: center;
  align-items: center;
  width: 100%;
  height: 100%;
  background: #f5f7fa;
  color: #c0c4cc;
  font-size: 14px;
}
.wrap {
  display: flex;
  .wrap-left {
    width: 380px;
    margin-right: 20px;
  }
  .wrap-right {
    flex: 1;
    min-width: 0;
    overflow: hidden;
  }
}
::-webkit-scrollbar {
  width: 6px;
  height: 6px;
}
::-webkit-scrollbar-thumb {
  border-radius: 6px;
}

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
    justify-content: space-between;
    align-items: center;
    border: 1px solid #dfe4ed;
    border-radius: 6px;

    .text {
      display: inline-block;
      width: 100%;
      text-align: center;
      padding: 5px 0px;
      font-size: 14px;
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
