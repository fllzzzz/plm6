<template>
  <div class="app-container wrap">
    <div class="wrap-left">
      <project-list ref="projectListRef" :maxHeight="maxHeight" @project-click="handleProjectClick" />
    </div>
    <div class="wrap-right">
      <el-tag
        v-show="
          !crud.query?.areaIds?.length &&
          !crud.query?.workshopIds?.length &&
          !crud.query.productionLineIds?.length &&
          !crud.query.groupsIds?.length
        "
        type="info"
        size="medium"
      >
        * 请先选择区域、车间、产线或者生产组进行零件排产
      </el-tag>
      <common-button
        v-show="
          !crud.query?.areaIds?.length &&
          !crud.query?.workshopIds?.length &&
          !crud.query.productionLineIds?.length &&
          !crud.query.groupsIds?.length
        "
        v-permission="permission.unProductDelRecord"
        type="info"
        class="filter-item"
        size="mini"
        icon="el-icon-refresh-left"
        @click="unProductRecordVisible = true"
        style="float: right"
      >
        剔除记录
      </common-button>
      <div
        v-show="
          crud.query?.areaIds?.length ||
          crud.query?.workshopIds?.length ||
          crud.query.productionLineIds?.length ||
          crud.query.groupsIds?.length
        "
      >
        <div class="head-container">
          <mHeader ref="headRef" @load="load" @change="handleChange">
            <template #searchRight>
              <span style="float: right">
                <common-button
                  v-permission="permission.unProductDel"
                  :type="isEliminateMode ? 'success' : 'danger'"
                  class="filter-item"
                  size="mini"
                  @click="handleEliminateModeChange"
                >
                  {{ isEliminateMode ? '关闭零件剔除' : '开启零件剔除' }}
                </common-button>
                <common-button
                  v-permission="permission.unProductDelRecord"
                  type="info"
                  class="filter-item"
                  size="mini"
                  icon="el-icon-refresh-left"
                  @click="unProductRecordVisible = true"
                >
                  剔除记录
                </common-button>
              </span>
            </template>
            <template #optLeft>
              <div style="display: flex">
                <el-checkbox
                  v-if="!isEliminateMode"
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
              <!-- <el-badge :value="padBlockData.length" class="item">
                <common-button type="primary" :loading="tableLoading" size="mini" class="filter-item" @click="padBlockClick">
                  标准零件列表
                </common-button>
              </el-badge> -->
              <template v-if="!isEliminateMode">
                <common-button v-permission="permission.add" type="success" class="filter-item" size="mini" @click="addPadBlock">
                  添加标准零件
                </common-button>
                <common-button
                  v-permission="permission.noNestingSave"
                  type="warning"
                  class="filter-item"
                  :disabled="Boolean(!checkedNodes?.length && padBlockData?.length)"
                  size="mini"
                  @click="unPreviewIt"
                >
                  无需套料保存
                </common-button>
                <common-button
                  v-permission="permission.nestingSave"
                  type="success"
                  class="filter-item"
                  size="mini"
                  :disabled="isIncludeDxf"
                  @click="previewIt"
                >
                  套料保存
                </common-button>
              </template>
              <el-tag size="medium" effect="plain" type="danger" style="margin-right: 5px">
                构件完成时间：{{ summaryInfo.minTime || '' }}~{{ summaryInfo.maxTime || '' }}
              </el-tag>
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
            <el-tooltip v-model="item.visibleTip" manual :open-delay="300" class="item" effect="light" placement="left-start">
              <template #content>
                <div style="display: flex">
                  <div style="display: flex; flex-direction: column; justify-content: center">
                    <p>{{ item.project?.shortName }}</p>
                    <p>编号：{{ item.serialNumber }}</p>
                    <p>长度：{{ item.length }}</p>
                    <p>单重：{{ item.netWeight }}</p>
                    <p>数量：{{ item.quantity }}</p>
                  </div>
                  <div style="flex: 1; display: flex">
                    <el-image style="flex: 1; width: 300px; height: 260px" :src="item.picturePath" z-index="999999" />
                  </div>
                </div>
              </template>
              <div
                class="board-box"
                style="position: relative; cursor: pointer"
                :style="{ 'background-color': `${item.boxColor}`, ...boxStyle }"
              >
                <div
                  style="display: flex; justify-content: space-between; width: 100%; align-items: center; padding: 0 5px"
                  @click.stop="item.visibleTip = !item.visibleTip"
                  @mouseleave="item.visibleTip = false"
                >
                  <el-checkbox
                    v-model="item.checked"
                    :disabled="!isEliminateMode ? Boolean(!item.imgLoad && item.picturePath) : false"
                    @click.stop
                    @change="handleCheckedChange($event, item)"
                  ></el-checkbox>
                  <span class="ellipsis-text text">
                    {{ item.serialNumber }}
                  </span>
                </div>
                <el-image
                  style="flex: 1; width: 95%"
                  :src="item.picturePath"
                  z-index="999999"
                  @dblclick.stop="item.visibleTip = !item.visibleTip"
                  @mouseleave="item.visibleTip = false"
                  @error="item.imgLoad = false"
                >
                  <template #error>
                    <div class="error-slot">
                      <span v-if="item.picturePath">加载失败</span>
                      <span v-else>未导入DXF</span>
                    </div>
                  </template>
                </el-image>
                <span class="ellipsis-text text" @click.stop="item.visibleTip = !item.visibleTip" @mouseleave="item.visibleTip = false">
                  <span style="color: #409eff">{{ item.specification }}</span
                  >/<span style="color: #42b983">{{ item.quantity }}</span>
                </span>
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
          @handleDel="handleDel"
          :thick-list="thickList"
          :material-list="materialList"
          :thick-data="crud.query.thickList"
        ></m-preview>
        <pad-block-dialog v-model:visible="dialogVisible" :pad-block-data="padBlockData" @addBlock="addBlock" />
        <!-- <pad-block ref="padBlockRef" v-model:visible="drawerVisible" :pad-block-data="padBlockData" /> -->
        <!-- 剔除操作 -->
        <eliminate-dialog v-model:visible="eliminateVisible" :info="partInfo" @close="handleEliminateCancel" @success="crud.refresh" />
        <!-- 剔除记录 -->
        <un-production-record v-model:visible="unProductRecordVisible" @refresh="crud.refresh" />
      </div>
    </div>
  </div>
</template>

<script setup>
import crudApi from '@/api/mes/scheduling-manage/machine-part'
import { computed, ref, onUnmounted, onMounted, nextTick, watch, provide } from 'vue'
import { ElMessage } from 'element-plus'
import { isNotBlank } from '@/utils/data-type'
import RAF from '@/utils/raf'
import { machinePartDxfTypeEnum, machinePartSchedulingTypeEnum as typeEnum } from '@enum-ms/mes'
import { machinePartSchedulingPM as permission } from '@/page-permission/mes'
import { parseTime } from '@/utils/date'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import mHeader from './module/header'
import mPreview from './module/preview'
import padBlockDialog from './module/pad-block-dialog'
// import padBlock from './module/pad-block'
import projectList from './module/project-list'
import eliminateDialog from './module/eliminate-dialog'
import unProductionRecord from './un-production-record'
import { deepClone } from '@/utils/data-type'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}
const thickList = ref([])
const materialList = ref([])
// const padBlockRef = ref()
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
    requiredQuery: ['monthList', 'material', 'thick']
  },
  tableRef
)

provide('permission', permission)

// 监听厚度 重置全选按钮
watch(
  () => crud.query.thick,
  (val) => {
    checkAll.value = false
  }
)
const { maxHeight } = useMaxHeight()

const boardList = ref([])
const summaryInfo = ref({ totalNetWeight: 0, quantity: 0, minTime: '', maxTime: '' })
const partInfo = ref({})
const isEliminateMode = ref(false)
const unProductRecordVisible = ref(false)
const eliminateVisible = ref(false)

CRUD.HOOK.handleRefresh = (crud, res) => {
  // const arr = []
  summaryInfo.value.totalNetWeight = res.data?.totalNetWeight || 0
  summaryInfo.value.quantity = res.data?.quantity || 0
  summaryInfo.value.minTime = parseTime(res.data?.minDate, '{y}-{m}-{d}')
  summaryInfo.value.maxTime = parseTime(res.data?.maxDate, '{y}-{m}-{d}')
  res.data.content = res.data.collect.map((v) => {
    if (checkedNodes.value.findIndex((k) => k.id === v.id) > -1 && !isEliminateMode.value) {
      // arr.push(v)
      v.checked = true
    } else {
      v.checked = false
    }
    v.visibleTip = false
    v.imgLoad = true
    return v
  })
  nestingLoading.value = false
}

function handleEliminateCancel() {
  partInfo.value.checked = false
}

function handleEliminateModeChange() {
  boardList.value.forEach((v) => {
    v.checked = false
  })
  isEliminateMode.value = !isEliminateMode.value
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

function handleProjectClick({ areaIds, workshopIds, productionLineIds, groupsIds }, month, configData) {
  crud.query.monthList = month
  crud.query.areaIds = areaIds
  if (configData === typeEnum.WORKSHOP.V) {
    crud.query.workshopIds = workshopIds
  }
  if (configData === typeEnum.PRODUCTION_LINE.V) {
    crud.query.productionLineIds = productionLineIds
  }
  if (configData === typeEnum.GROUPS.V) {
    crud.query.groupsIds = groupsIds
  }
  const arr = []
  checkedNodes.value.forEach((v) => {
    if (areaIds.indexOf(String(v.areaId)) > -1 || areaIds.indexOf(v.areaId) > -1) {
      arr.push(v)
    }
  })
  checkedNodes.value = arr
  nextTick(() => {
    headRef.value?.refreshConditions()
  })
}

async function handleSaveSuccess() {
  const lastQuery = deepClone(crud.query)
  checkAll.value = false
  checkedNodes.value = []
  padBlockData.value = []
  boardList.value = []
  crud.page.page = 1
  summaryInfo.value = { totalNetWeight: 0, quantity: 0 }
  // await projectListRef?.value?.refresh(lastQuery)
  await headRef.value?.refreshConditions(lastQuery)
}

function handleDel(row, val) {
  if (row.needMachinePartLinkList?.length) {
    handleCheckedChange(false, { id: val })
  } else {
    const padIdx = padBlockData.value.findIndex((v) => v.id === val)
    padBlockData.value.splice(padIdx, 1)
  }
  const delIndex = previewList.value.findIndex((v) => v.id === val)
  previewList.value.splice(delIndex, 1)
  previewList.value = [...checkedNodes.value, ...padBlockData.value]
}

// --------------------------- 选择操作 start ------------------------------

const checkAll = ref(false)
const checkedNodes = ref([])

// 全选只要含有未导入dxf 只能进行无需套料
const isIncludeDxf = computed(() => {
  let flag = false
  checkedNodes.value.forEach((v) => {
    if (!v.picturePath) {
      flag = true
    }
  })
  return flag
})

// 切换dxf是否导入 清除全选
function boolDxfChange() {
  handleCheckedAll()
  checkAll.value = false
  crud.toQuery()
}

// 切换项目清除选择
// function saveCheck() {
// }
function handleCheckedChange(value, item) {
  if (isEliminateMode.value && value) {
    partInfo.value = item
    eliminateVisible.value = true
  }
  // saveCheck()
  const _checkedIndex = checkedNodes.value.findIndex((v) => v.id === item.id)
  if (value) {
    if (_checkedIndex === -1) checkedNodes.value.push(item)
  } else {
    if (_checkedIndex > -1) checkedNodes.value.splice(_checkedIndex, 1)
  }
  console.log(item, 'handleCheckedChange')
  boardList.value.forEach((v) => {
    if (v.id === item.id) {
      v.checked = value
    }
  })
  let isNext = false
  checkedNodes.value.forEach((v) => {
    if (!v.imgLoad || !v.picturePath) {
      isNext = true
      return false
    }
  })
  nestingLoading.value = isNext
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
// const drawerVisible = ref(false)
// const tableLoading = ref(false)
const padBlockData = ref([])
// function padBlockClick() {
//   drawerVisible.value = true
// }

function addBlock(val) {
  const findVal = padBlockData.value.find((v) => v.id === val.id)
  console.log(val, findVal, 'val')
  if (isNotBlank(findVal)) {
    findVal.usedQuantity += val.usedQuantity
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
