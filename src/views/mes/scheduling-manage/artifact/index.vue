<template>
  <div class="app-container wrap">
    <div class="wrap-left">
      <project-to-area-tree :heightStyle="heightStyle" @area-click="handleAreaClick" />
    </div>
    <div class="wrap-right">
      <el-tag v-show="!crud.query?.areaIdList?.length" type="info" size="medium"> * 请先选择区域，进行构件区域排产 </el-tag>
      <div v-show="crud.query?.areaIdList?.length">
        <div class="head-container">
          <mHeader ref="mHeaderRef">
            <template #optLeft>
              <common-button v-permission="permission.save" type="success" size="mini" @click="previewIt">预览并保存</common-button>
              <common-button
                :disabled="crud.query?.areaIdList?.length !== 1"
                v-if="checkPermission(permission.import)"
                type="primary"
                size="mini"
                @click="taskImportVisible = true"
              >
                任务导入
              </common-button>
              <export-button
                v-if="checkPermission(permission.import)"
                :fn="downloadTemplate"
                :disabled="crud.query?.areaIdList?.length !== 1"
                style="margin-left: 10px"
                :params="{ areaId: crud.query?.areaIdList?.[0], ...queryParams }"
              >
                任务导入模板下载
              </export-button>
            </template>
            <template #viewLeft>
              <el-tag size="medium" effect="plain" style="margin-right: 5px"> 数量(件)：{{ summaryInfo.quantity || 0 }} </el-tag>
              <el-tag size="medium" effect="plain" style="margin-right: 10px">
                重量(kg)：{{ summaryInfo.totalNetWeight?.toFixed(2) || 0 }}
              </el-tag>
              <el-badge :value="totalBadge" :max="99" :hidden="totalBadge < 1" style="margin-right: 20px">
                <common-button v-permission="permission.recordGet" type="primary" size="mini" @click="previewRecord">
                  构件排产记录
                </common-button>
              </el-badge>
            </template>
          </mHeader>
        </div>
        <!--表格渲染-->
        <common-table
          ref="tableRef"
          v-loading="crud.loading"
          :data="crud.data"
          :empty-text="crud.emptyText"
          :max-height="maxHeight - 110"
          row-key="rowKey"
          :cell-class-name="wrongCellMask"
          style="width: 100%"
          @selection-change="crud.selectionChangeHandler"
        >
          <el-table-column type="selection" width="55" align="center" />
          <el-table-column label="序号" type="index" align="center" width="60" />
          <productType-base-info-columns :productType="productType" snClickable @drawingPreview="drawingPreview" :columns="columns" />
          <el-table-column
            v-if="columns.visible('netWeight')"
            :show-overflow-tooltip="true"
            prop="netWeight"
            :label="`单净重(kg)`"
            min-width="80px"
            align="center"
          >
            <template #default="{ row }">
              <span>{{ row.netWeight }}</span>
            </template>
          </el-table-column>
          <el-table-column
            v-if="columns.visible('needSchedulingQuantity')"
            :show-overflow-tooltip="true"
            prop="needSchedulingQuantity"
            label="数量"
            min-width="120px"
            align="center"
          >
            <template #default="{ row: { sourceRow: row } }">
              <el-input-number
                v-model="row.needSchedulingQuantity"
                :step="1"
                :min="1"
                :max="row.unSchedulingQuantity"
                :precision="0"
                size="mini"
                controls-position="right"
                style="width: 100%"
              />
            </template>
          </el-table-column>
          <el-table-column
            v-if="columns.visible('groupsId')"
            :show-overflow-tooltip="true"
            prop="groupsId"
            label="生产组"
            min-width="150px"
            align="center"
          >
            <template #default="{ row: { sourceRow: row }, $index }">
              <el-cascader
                :ref="(el) => (cascaderRef[$index] = el)"
                v-model="row.groupsId"
                :options="groupsTree"
                :props="{
                  value: 'id',
                  label: 'name',
                  children: 'children',
                  expandTrigger: 'hover',
                  emitPath: false,
                }"
                :show-all-levels="false"
                filterable
                clearable
                :placeholder="$index === 0 ? '请选择生产组' : '同上'"
                @expand-change="handleExpandChange($event, row, $index, cascaderRef[$index])"
                @focus="handleFocusChange($event, row, $index, cascaderRef[$index])"
                @change="handleGroupsChange($event, row, $index)"
              />
            </template>
          </el-table-column>
          <el-table-column
            v-if="columns.visible('askCompleteTime')"
            prop="askCompleteTime"
            label="要求完成日期"
            align="center"
            min-width="130px"
          >
            <template #default="{ row: { sourceRow: row }, $index }">
              <el-date-picker
                v-model="row.askCompleteTime"
                type="date"
                size="mini"
                value-format="x"
                style="width: 100%"
                :disabledDate="(v) => moment(v).valueOf() < moment().subtract(1, 'days').valueOf()"
                :placeholder="$index === 0 ? '需求完成日期' : '同上'"
                @change="handleAskCompleteTimeChange($event, row, $index)"
              />
            </template>
          </el-table-column>
        </common-table>
        <!--分页组件-->
        <pagination />
      </div>
      <!-- pdf预览 -->
      <bim-preview-drawer
        v-model:visible="showDrawing"
        :bool-bim="drawingRow?.boolBim"
        :drawingSN="drawingRow?.drawingSN"
        :monomer-id="drawingRow?.monomerId"
        :area-id="crud.query.areaId"
        :serial-number="drawingRow?.serialNumber"
        :productId="drawingRow?.productId"
        :productType="drawingRow?.productType"
      />
      <mPreview v-model:visible="previewVisible" :productType="productType" :list="submitList" @success="refresh(true)"></mPreview>
      <preview-summary-detail
        v-model:visible="previewSummaryVisible"
        :other-query="{
          areaIdList: crud.query.areaIdList,
        }"
        @refresh="refresh(true)"
      />
      <task-import-dialog v-model:visible="taskImportVisible" :query="crud.query" :productType="productType" @success="refresh(true)" />
    </div>
  </div>
</template>

<script setup>
import crudApi, { getSummary, getBadgeNum } from '@/api/mes/scheduling-manage/artifact'
import { downloadTemplate } from '@/api/mes/scheduling-manage/common'
import { ref, provide, computed } from 'vue'
import { ElMessage, ElMessageBox } from 'element-plus'
import moment from 'moment'

// import { deepClone } from '@data-type/index'
import { componentTypeEnum } from '@enum-ms/mes'
import { positiveNumPattern } from '@/utils/validate/pattern'
import checkPermission from '@/utils/system/check-permission'
import { artifactSchedulingPM as permission } from '@/page-permission/mes'

import useSchedulingGroups from '@compos/mes/scheduling/use-scheduling-groups'
import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import useTableValidate from '@compos/form/use-table-validate'
import pagination from '@crud/Pagination'
import ExportButton from '@comp-common/export-button/index.vue'
import mHeader from './module/header'
import mPreview from './module/preview'
import taskImportDialog from './module/task-import-dialog'
import previewSummaryDetail from './module/preview-summary-detail'
import projectToAreaTree from './module/project-to-area-tree'
import productTypeBaseInfoColumns from '@comp-mes/table-columns/productType-base-info-columns'
import useDrawing from '@compos/use-drawing'
import bimPreviewDrawer from '@/components-system/bim/bim-preview-drawer'
import { debounce } from '@/utils'

const productType = componentTypeEnum.ARTIFACT.V
provide('productType', productType)

const mHeaderRef = ref()
const curFactoryIds = ref([])
const curWorkshopIds = ref([])
const curAreaIdObj = ref({})
const curMainScheduleDate = ref()
const submitList = ref([])
const previewVisible = ref(false)
const previewSummaryVisible = ref(false)

const { showDrawing, drawingRow, drawingPreview } = useDrawing({ pidField: 'productId', productTypeField: 'ARTIFACT' })

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false,
}

const totalBadge = ref()
const tableRef = ref()
const { crud, columns, CRUD } = useCRUD(
  {
    title: '构件排产',
    sort: [],
    permission: { get: permission.get },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    queryOnPresenterCreated: false,
    requiredQuery: ['areaIdList', 'structureClassId'],
  },
  tableRef
)

const { maxHeight, heightStyle } = useMaxHeight({ paginate: true })

const cascaderRef = ref([])
const taskImportVisible = ref(false)
const summaryInfo = ref({})
const queryParams = computed(() => {
  return {
    productType: productType,
    structureClassId: crud.query.structureClassId,
  }
})
const { groupsTree, groupsObj } = useSchedulingGroups({ queryParams, factoryIds: curFactoryIds })
provide('areaIdObj', curAreaIdObj)
provide('curFactoryIds', curFactoryIds)

// --------------------------- 设置级联数据默认展开第一个【防止面板跳来跳去】 start ------------------------------

function handleFocusChange(expend, row, index, curCascaderRef) {
  if (!row.groupsId || row.groupsId === '同上') {
    const menus = curCascaderRef.panel.menuList[0]
    setCascaderExpandNode(menus, menus.nodes[0])
  }
}

function setCascaderExpandNode(menus, node) {
  if (!node.isLeaf) {
    menus.panel.expandNode(node, true)
    if (!node.children[0].isLeaf) {
      setCascaderExpandNode(menus, node.children[0])
    }
  }
}

function handleExpandChange(expend, row, index, curCascaderRef) {
  const menus = curCascaderRef.panel.menuList[0]
  const curExpandingNode = menus.panel.expandingNode
  if (!curExpandingNode.children[0].isLeaf) {
    setCascaderExpandNode(menus, curExpandingNode.children[0])
  }
}

// --------------------------- 设置级联数据默认展开第一个【防止面板跳来跳去】 end --------------------------------

const tableRules = {
  needSchedulingQuantity: [
    { required: true, message: '请填写数量', trigger: 'blur' },
    { pattern: positiveNumPattern, message: '数量必须大于0', trigger: 'blur' },
  ],
  groupsId: [{ required: true, message: '请选择生产组', trigger: 'change' }],
  askCompleteTime: [{ required: true, message: '请选择需求完成日期', trigger: 'change' }],
}
const ditto = new Map([
  ['groupsId', '同上'],
  ['askCompleteTime', '同上'],
])
const { tableValidate, cleanUpData, wrongCellMask } = useTableValidate({ rules: tableRules, ditto })

CRUD.HOOK.beforeToQuery = () => {
  fetchSummary()
  schedulingNumGet()
}

CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data.content.map((v, i) => {
    v.needSchedulingQuantity = v.unSchedulingQuantity // 需要排产的数量
    v.factoryId = curAreaIdObj.value[v.area?.id]?.factoryId
    if (i > 0) {
      v.askCompleteTime = '同上'
      v.groupsId = '同上'
    }
    v.rowKey = i + '' + Math.random()
    return v
  })
}

async function fetchSummary() {
  summaryInfo.value = { totalNetWeight: 0, quantity: 0 }
  if (!crud.query.structureClassId) return
  try {
    summaryInfo.value = (await getSummary(crud.query)) || {}
  } catch (error) {
    console.log(error, '获取汇总信息')
  }
}

function handleGroupsChange(val, row, index) {
  if (index !== 0 && !val) {
    row.groupsId = '同上'
  }
}

function handleAskCompleteTimeChange(val, row, index) {
  if (index !== 0 && !val) {
    row.askCompleteTime = '同上'
  }
}

function refresh(isRefreshTypeList = false) {
  if (isRefreshTypeList) {
    mHeaderRef.value?.refreshTypeList()
  }
  fetchSummary()
  schedulingNumGet()
  crud.toQuery()
}

async function schedulingNumGet() {
  try {
    const data = await getBadgeNum({ areaIdList: crud.query.areaIdList })
    totalBadge.value = data
  } catch (error) {
    console.log('获取构件排产记录气泡条数失败', error)
  }
}

const handleAreaClick = debounce(function (nodes = []) {
  console.log(nodes, 'handleAreaClick')
  summaryInfo.value = {}
  totalBadge.value = 0
  const _areaIds = []
  const _areaIdObj = {}
  const _factoryIds = []
  const _workshopIds = []
  curMainScheduleDate.value = undefined
  for (let x = 0; x < nodes.length; x++) {
    _areaIds.push(nodes[x].id)
    _areaIdObj[nodes[x].id] = nodes[x]
    _factoryIds.push(nodes[x].factoryId)
    _workshopIds.push(nodes[x].workshopId)
    curMainScheduleDate.value = curMainScheduleDate.value ? Math.min(nodes[x].endDate, curMainScheduleDate.value) : nodes[x].endDate
  }
  crud.query.areaIdList = _areaIds
  crud.query.structureClassId = undefined
  curAreaIdObj.value = _areaIdObj
  curFactoryIds.value = _factoryIds
  curWorkshopIds.value = _workshopIds
  schedulingNumGet()
  fetchSummary()
  mHeaderRef.value?.refreshTypeList()
  crud.toQuery()
}, 500)

async function previewIt() {
  if (!crud.selections?.length) {
    ElMessage.warning('请至少选择一条数据')
    return
  }
  // const _list = deepClone(crud.selections)
  const _list = crud.selections.map((v) => v)
  const { validResult, dealList } = tableValidate(_list)
  if (validResult) {
    cleanUpData(dealList) // 同上赋值
    let timeFlag = false
    submitList.value = dealList.map((v, i) => {
      if (v.askCompleteTime > curMainScheduleDate.value) {
        timeFlag = true
      }
      return {
        ...v,
        ...groupsObj.value[v.groupsId],
      }
    })
    if (timeFlag) {
      try {
        await ElMessageBox.confirm('排产计划大于排期计划，是否确定？', '提示', {
          confirmButtonText: '确定',
          cancelButtonText: '取消',
          type: 'warning',
        })
        previewVisible.value = true
      } catch (er) {
        console.log(er, '取消')
      }
      return
    }
  } else {
    return validResult
  }

  previewVisible.value = true
}

function previewRecord() {
  previewSummaryVisible.value = true
}
</script>

<style lang="scss" scoped>
.wrap {
  display: flex;
  .wrap-left {
    width: 300px;
    margin-right: 10px;
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
</style>
