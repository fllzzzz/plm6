<template>
  <div class="app-container wrap">
    <div class="wrap-left">
      <project-list ref="projectListRef" :maxHeight="maxHeight" @project-click="handleProjectClick" />
    </div>
    <div class="wrap-right">
      <el-tag v-show="!crud.query?.areaIds?.length" type="info" size="medium"> * 请先选择区域，进行零件排产 </el-tag>
      <div v-show="crud.query?.areaIds?.length">
        <div class="head-container">
          <mHeader ref="headRef" @load="load" @change="handleChange">
            <template #optLeft>
              <div style="display: flex">
                <el-checkbox
                  v-model="checkAll"
                  size="small"
                  style="margin-right: 5px"
                  :indeterminate="checkedNodes.length > 0 && checkedNodes.length !== boardList && !checkAll"
                  border
                  @change="handleCheckedAll"
                  >全选</el-checkbox
                >
                <common-radio-button
                  v-model="boolDxfEnum"
                  :options="machinePartDxfTypeEnum.ENUM"
                  showOptionAll
                  type="enum"
                  class="filter-item"
                  @change="boolDxfChange"
                />
              </div>
            </template>
            <template #viewLeft>
              <el-badge :value="padBlockData.length" class="item">
                <common-button type="primary" :loading="tableLoading" size="mini" class="filter-item" @click="padBlockClick">
                  标准零件列表
                </common-button>
              </el-badge>
              <common-button v-permission="permission.save" type="success" class="filter-item" size="mini" @click="addPadBlock">
                添加标准零件
              </common-button>
              <common-button
                v-permission="permission.save"
                type="warning"
                class="filter-item"
                :disabled="Boolean(!checkedNodes?.length && padBlockData?.length)"
                size="mini"
                @click="unPreviewIt"
              >
                无需套料保存
              </common-button>
              <common-button
                v-permission="permission.save"
                type="success"
                class="filter-item"
                size="mini"
                :disabled="crud.query.boolDxfEnum === machinePartDxfTypeEnum.UN_EXPORT.V || nestingLoading === true"
                @click="previewIt"
              >
                套料保存
              </common-button>
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
                @click.stop="item.visibleTip = !item.visibleTip"
              >
                <div style="display: flex; justify-content: space-between; width: 100%; align-items: center; padding: 0 5px">
                  <el-checkbox
                    v-model="item.checked"
                    :disabled="Boolean(!item.imgLoad && item.picturePath)"
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
                  ><span style="color: #409eff">{{ item.specification }}</span
                  >/<span style="color: #42b983">{{ item.quantity }}</span></span
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
        <m-preview
          v-model:visible="previewVisible"
          :list="previewList"
          :type="type"
          :checked-nodes="checkedNodes"
          :pad-block-data="padBlockData"
          @success="handleSaveSuccess"
          :thick-list="thickList"
          :material-list="materialList"
        ></m-preview>
        <pad-block-dialog v-model:visible="dialogVisible" :pad-block-data="padBlockData" @addBlock="addBlock" />
        <pad-block v-model:visible="drawerVisible" :pad-block-data="padBlockData" />
      </div>
    </div>
  </div>
</template>

<script setup>
import crudApi from '@/api/mes/scheduling-manage/machine-part'
import { computed, ref, onUnmounted, onMounted, nextTick } from 'vue'
import { ElMessage } from 'element-plus'
import { isNotBlank } from '@/utils/data-type'
import RAF from '@/utils/raf'
import { machinePartDxfTypeEnum } from '@enum-ms/mes'
import { machinePartSchedulingPM as permission } from '@/page-permission/mes'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import mHeader from './module/header'
import mPreview from './module/preview'
import padBlockDialog from './module/pad-block-dialog'
import padBlock from './module/pad-block'
import projectList from './module/project-list'
import { deepClone } from '@/utils/data-type'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}
const thickList = ref([])
const materialList = ref([])
const boolDxfEnum = ref()
const projectListRef = ref()
const tableRef = ref()
const nestingLoading = ref(false)
const type = ref()
const previewList = ref([])
const { crud, CRUD } = useCRUD(
  {
    title: '零件排产',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    queryOnPresenterCreated: false,
    hasPagination: false,
    requiredQuery: ['monthList', 'material', 'areaIds', 'thickList']
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

function handleChange(matVal, thickVal) {
  materialList.value = matVal
  thickList.value = thickVal
}

CRUD.HOOK.beforeRefresh = () => {
  crud.query.boolDxfEnum = boolDxfEnum.value
  boardList.value = []
  summaryInfo.value = { totalNetWeight: 0, quantity: 0 }
}

CRUD.HOOK.afterRefresh = () => {
  crud.data.forEach((component) => {
    boardList.value.push(component)
  })
}

// --------------------------- end --------------------------------

function handleProjectClick({ areaIds }, month) {
  crud.query.monthList = month
  crud.query.areaIds = areaIds
  nextTick(() => {
    headRef.value?.refreshConditions()
  })
}

async function handleSaveSuccess() {
  const lastQuery = deepClone(crud.query)
  checkAll.value = false
  boardList.value = []
  crud.page.page = 1
  padBlockData.value = []
  await projectListRef?.value?.refresh(lastQuery)
  await headRef.value?.refreshConditions(lastQuery)
}

// --------------------------- 选择操作 start ------------------------------

const checkAll = ref(false)
const checkedNodes = ref([])

// 切换dxf是否导入 清除全选
function boolDxfChange() {
  handleCheckedAll()
  checkAll.value = false
  crud.toQuery()
}

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
  if (value && !item.picturePath) {
    item.checked = value
    nestingLoading.value = true
  } else {
    item.checked = value
    nestingLoading.value = false
  }
}
function handleCheckedAll(val) {
  checkAll.value = val
  boardList.value.forEach((v) => {
    if (v.imgLoad || !v.picturePath) {
      v.checked = val
      handleCheckedChange(val, v)
    } else if (v.imgLoad || v.picturePath) {
      v.checked = val
      handleCheckedChange(val, v)
    }
  })
  nestingLoading.value = val
}

// --------------------------- 选择操作 end --------------------------------

// --------------------------- 预览并保存 start ------------------------------

const previewVisible = ref(false)

function previewIt() {
  type.value = 1
  if (padBlockData.value?.length === 0 && !checkedNodes.value?.length) {
    ElMessage.warning('请至少选择一条数据')
    return
  }
  previewList.value = [...checkedNodes.value, ...padBlockData.value]
  previewVisible.value = true
}
function unPreviewIt() {
  type.value = 0
  if (padBlockData.value?.length === 0 && !checkedNodes.value?.length) {
    ElMessage.warning('请至少选择一条数据')
    return
  }
  previewList.value = [...checkedNodes.value, ...padBlockData.value]
  previewVisible.value = true
}
// --------------------------- 预览并保存 end --------------------------------

//  添加标准零件弹窗
const dialogVisible = ref(false)

function addPadBlock() {
  dialogVisible.value = true
}

// 标准零件列表
const drawerVisible = ref(false)
const tableLoading = ref(false)
const padBlockData = ref([])
function padBlockClick() {
  drawerVisible.value = true
}

function addBlock(val) {
  const findVal = padBlockData.value.find((v) => v.id === val.id)
  if (isNotBlank(findVal)) {
    findVal.quantity += val.quantity
  } else {
    padBlockData.value.push(JSON.parse(JSON.stringify(val)))
  }
}
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
    width: 320px;
    margin-right: 10px;
  }
  .wrap-right {
    flex: 1;
    min-width: 0;
    overflow: hidden;
  }
}
.item {
  margin-right: 10px;
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
