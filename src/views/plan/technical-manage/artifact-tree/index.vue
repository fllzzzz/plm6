<template>
  <div class="app-container">
    <template v-if="pageShow">
      <!--工具栏-->
      <div class="head-container">
        <mHeader :project-id="globalProjectId" @getAreaData="getAreaData" :globalProject="globalProject"/>
      </div>
      <!--表格渲染-->
      <common-table
        ref="tableRef"
        v-loading="crud.loading"
        :data="crud.data"
        :empty-text="crud.emptyText"
        :max-height="maxHeight"
        :tree-props="{ children: 'children', hasChildren: 'hasChildren' }"
        :row-class-name="handleRowClassName"
        :cell-class-name="cellClassName"
        row-key="rowKey"
        style="width: 100%"
        :stripe="false"
        lazy
        :load="load"
        @sort-change="crud.handleSortChange"
        @selection-change="crud.selectionChangeHandler"
        @row-dblclick="dbclick"
        return-source-data
        :showEmptySymbol="false"
      >
        <el-table-column key="selection" type="selection" width="55" />
        <el-table-column v-if="columns.visible('index')" prop="index" label="序号" align="center" width="80">
          <template v-slot="scope">
            <span v-if="scope.row.children">{{ changeIndex(scope.row) }}</span>
            <span v-else class="child">{{ changeIndex(scope.row) }}</span>
          </template>
        </el-table-column>
        <!-- <el-table-column
          v-if="columns.visible('assembleSerialNumberList')"
          key="assembleSerialNumberList"
          prop="assembleSerialNumberList"
          sortable="custom"
          :show-overflow-tooltip="true"
          label="组立号"
          min-width="100px"
        >
          <template v-slot="scope">
            <span>{{
              scope.row.assembleSerialNumberList && scope.row.assembleSerialNumberList.length > 0
                ? scope.row.assembleSerialNumberList.join(',')
                : ''
            }}</span>
          </template>
        </el-table-column> -->
        <el-table-column
          v-if="columns.visible('name')"
          key="name"
          prop="name"
          sortable="custom"
          :show-overflow-tooltip="true"
          label="名称"
          min-width="100px"
        />
        <el-table-column
          v-if="columns.visible('serialNumber')"
          key="serialNumber"
          prop="serialNumber"
          sortable="custom"
          :show-overflow-tooltip="true"
          label="编号"
          min-width="140px"
        >
          <template #header>
            <el-tooltip class="item" effect="light" :content="`双击编号可预览图纸`" placement="top">
              <div style="display: inline-block">
                <span style="margin-left: 10px">编号</span>
                <i class="el-icon-info" />
              </div>
            </el-tooltip>
          </template>
          <template v-slot="scope">
            <table-cell-tag :show="scope.row.dataType !== 2 && scope.row.boolSendDirectly" name="直发件" />
            <!-- relationType=8 显示配套件标记 -->
            <table-cell-tag :show="scope.row.dataType !== 2 && scope.row.relationType === 8" name="配套件" />
            <span style="cursor: pointer; margin-left: 10px" @dblclick="drawingPreview(scope.row)">{{ scope.row.serialNumber }}</span>
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('specification')"
          key="specification"
          prop="specification"
          sortable="custom"
          :show-overflow-tooltip="true"
          label="规格"
          min-width="120"
        >
          <template v-slot="scope">
            {{ scope.row.specification ? scope.row.specification : '-' }}
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('length')"
          key="length"
          prop="length"
          sortable="custom"
          :show-overflow-tooltip="true"
          :label="`长度\n(mm)`"
          align="left"
          min-width="85px"
        >
          <template v-slot="scope">
            {{ scope.row.length ? scope.row.length.toFixed(DP.MES_ARTIFACT_L__MM) : '-' }}
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('material')"
          key="material"
          prop="material"
          sortable="custom"
          :show-overflow-tooltip="true"
          label="材质"
          align="center"
          min-width="80px"
        >
          <template v-slot="scope">
            {{ scope.row.material ? scope.row.material : '-' }}
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('quantity')"
          key="quantity"
          prop="quantity"
          sortable="custom"
          label="数量"
          align="left"
          min-width="80px"
        >
          <template v-slot="scope">
            {{ scope.row.quantity ? scope.row.quantity : '-' }}
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('netWeight')"
          key="netWeight"
          prop="netWeight"
          sortable="custom"
          :show-overflow-tooltip="true"
          :label="`单净重\n(kg)`"
          align="left"
          min-width="80px"
        >
          <template v-slot="scope">
            {{ scope.row.netWeight ? scope.row.netWeight.toFixed(DP.COM_WT__KG) : '-' }}
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('grossWeight')"
          key="grossWeight"
          prop="grossWeight"
          sortable="custom"
          :show-overflow-tooltip="true"
          :label="`单毛重\n(kg)`"
          align="left"
          min-width="80px"
        >
          <template v-slot="scope">
            {{ scope.row.grossWeight ? scope.row.grossWeight.toFixed(DP.COM_WT__KG) : '-' }}
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('totalNetWeight')"
          key="totalNetWeight"
          prop="totalNetWeight"
          :show-overflow-tooltip="true"
          :label="`总净重\n(kg)`"
          align="left"
          min-width="95px"
        >
          <template v-slot="scope">
            {{ scope.row.totalNetWeight ? scope.row.totalNetWeight.toFixed(DP.COM_WT__KG) : '-' }}
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('totalGrossWeight')"
          key="totalGrossWeight"
          prop="totalGrossWeight"
          :show-overflow-tooltip="true"
          :label="`总毛重\n(kg)`"
          align="left"
          min-width="95px"
        >
          <template v-slot="scope">
            {{ scope.row.totalGrossWeight ? scope.row.totalGrossWeight.toFixed(DP.COM_WT__KG) : '-' }}
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('surfaceArea')"
          key="surfaceArea"
          prop="surfaceArea"
          sortable="custom"
          :label="`面积\n(㎡)`"
          align="left"
          min-width="80px"
        >
          <template v-slot="scope">
            {{ scope.row.surfaceArea ? scope.row.surfaceArea.toFixed(DP.COM_AREA__M2) : '-' }}
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('drawingNumber')"
          key="drawingNumber"
          prop="drawingNumber"
          sortable="custom"
          :show-overflow-tooltip="true"
          label="图号"
          min-width="100px"
        >
          <template v-slot="scope">
            {{ scope.row.drawingNumber ? scope.row.drawingNumber : '-' }}
          </template>
        </el-table-column>
        <el-table-column v-if="columns.visible('remark')" key="remark" prop="remark" :show-overflow-tooltip="true" label="备注" width="350px">
          <template v-slot:header>
            <el-tooltip
              class="item"
              effect="light"
              :content="`双击备注可修改`"
              placement="top"
            >
              <div style="display:inline-block;">
                <span>备注</span>
                <i class="el-icon-info" />
              </div>
            </el-tooltip>
          </template>
          <template v-slot="scope">
            <template v-if="scope.row.dataType===2">
              <span v-if="!scope.row.edit">{{ scope.row.remark || '-'}}</span>
              <span v-else>
                <el-input
                  v-model="scope.row.remark"
                  type="textarea"
                  :maxlength="64"
                  clearable
                  :rows="1"
                  size="mini"
                  style="width:180px;"
                />
                <el-button size="mini" type="primary" :loading="scope.row.editLoading" @click="saveIt(scope.row)">保存</el-button>
                <el-button size="mini" type="info" @click="cancelIt(scope.row)">取消</el-button>
              </span>
            </template>
            <template v-else>-</template>
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('userName')"
          key="userName"
          prop="userName"
          :show-overflow-tooltip="true"
          label="导入人"
          min-width="110"
        >
          <template v-slot="scope">
            {{ scope.row.userName ? scope.row.userName : '-' }}
          </template>
        </el-table-column>
        <el-table-column v-if="columns.visible('createTime')" key="createTime" prop="createTime" label="上传时间" min-width="160px">
          <template v-slot="scope">
            <div>{{ scope.row.createTime ? parseTime(scope.row.createTime, '{y}-{m}-{d}') : '-' }}</div>
          </template>
        </el-table-column>
        <el-table-column v-if="columns.visible('status')" key="status" prop="status" label="状态" align="center" width="80px" fixed="right">
          <template v-slot="scope">
            <el-switch
              v-model="scope.row.boolStatusEnum"
              :disabled="!checkPermission(permission.edit)"
              active-color="#13ce66"
              :active-value="true"
              :inactive-value="false"
              @change="changeStatus(scope.row, scope.row.boolStatusEnum)"
            />
          </template>
        </el-table-column>
        <!--编辑与删除-->
        <el-table-column
          v-if="checkPermission([...permission.editNum, ...permission.productionStatus])"
          label="操作"
          width="160px"
          align="center"
          fixed="right"
        >
          <template v-slot="scope">
            <template v-if="scope.row.dataType === 2">
              <common-button size="mini" @click="handleNum(scope.row)" icon="el-icon-edit" v-permission="permission.editNum" />
              <common-button
size="mini"
@click="viewState(scope.row)"
v-permission="permission.productionStatus"
                ><svg-icon
icon-class="document"
              /></common-button>
            </template>
          </template>
        </el-table-column>
      </common-table>
      <!--分页组件-->
      <pagination />
      <changeForm v-model="numVisible" :detailInfo="currentRow" @success="handleSuccess" />
      <productionState v-model="stateVisible" :detailInfo="currentRow" @success="handleSuccess" />
      <!-- pdf预览 -->
      <bim-preview-drawer
        v-model:visible="showBimDialog"
        :bool-bim="drawingRow?.boolBim"
        :drawingSN="drawingRow?.drawingSN"
        :monomer-id="drawingRow?.monomerId"
        :area-id="crud.query.areaId"
        :serial-number="drawingRow?.serialNumber"
        :productId="drawingRow?.productId"
        :productType="drawingRow?.productType"
      />
      <!-- pdf预览 -->
      <drawing-preview-fullscreen-dialog
        v-model="showDrawingDialog"
        :drawingSN="drawingRow?.drawingSN"
        :bool-bim="drawingRow?.boolBim"
        :serial-number="drawingRow?.serialNumber"
        :productId="drawingRow?.productId"
        :productType="drawingRow?.productType"
      />
    </template>
    <template v-else>
      <span style="color: red; font-size: 13px">当前项目内容没有包含构件,请到合同管理中进行配置</span>
    </template>
  </div>
</template>

<script setup>
import crudApi, { editStatus, artifactPart, editRemark } from '@/api/plan/technical-manage/artifact-tree'
import { ref, nextTick, watch } from 'vue'
import { artifactTreePM as permission } from '@/page-permission/plan'
import checkPermission from '@/utils/system/check-permission'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import useDrawing from '@compos/use-drawing'
import { DP } from '@/settings/config'
import { ElMessageBox } from 'element-plus'
import { parseTime } from '@/utils/date'
import { mapGetters } from '@/store/lib'
import { componentTypeEnum } from '@enum-ms/mes'
import { TechnologyTypeAllEnum } from '@enum-ms/contract'

import pagination from '@crud/Pagination'
import mHeader from './module/header'
import changeForm from './module/change-form'
import productionState from './module/production-state'
import bimPreviewDrawer from '@/components-system/bim/bim-preview-drawer'
import drawingPreviewFullscreenDialog from '@comp-base/drawing-preview/drawing-preview-fullscreen-dialog'
import tableCellTag from '@comp-common/table-cell-tag/index.vue'

const { globalProject, globalProjectId } = mapGetters(['globalProject', 'globalProjectId'])
const { showDrawing, drawingRow, drawingPreview } = useDrawing({ pidField: 'id', typeField: 'productType' })

const showBimDialog = ref(false)
const showDrawingDialog = ref(false)
const pageShow = ref(true)

watch([() => showBimDialog.value, () => showDrawingDialog.value], ([b, d]) => {
  if (!b && !d) {
    showDrawing.value = false
  }
  console.log(b, d, showDrawing.value, 'show')
})

watch(
  () => showDrawing.value,
  (val) => {
    if (val) {
      if (drawingRow.value?.productType && drawingRow.value?.productType & componentTypeEnum.ARTIFACT.V) {
        showBimDialog.value = true
      }
      if (drawingRow.value?.productType && !(drawingRow.value?.productType & componentTypeEnum.ARTIFACT.V)) {
        showDrawingDialog.value = true
      }
    }
  }
)

watch(
  () => globalProject.value,
  (val) => {
    if (globalProject.value.projectContentList?.length > 0) {
      pageShow.value = globalProject.value.projectContentList.findIndex((v) => v.no === TechnologyTypeAllEnum.STRUCTURE.V) > -1
    } else {
      pageShow.value = false
    }
  },
  { deep: true, immediate: true }
)

const optShow = {
  add: false,
  edit: false,
  del: true,
  download: false
}

const tableRef = ref()
const currentRow = ref({})
const numVisible = ref(false)
const stateVisible = ref(false)
const allArea = ref([])
const { crud, columns, CRUD } = useCRUD(
  {
    title: '零构件清单',
    sort: ['id.asc'],
    permission: { ...permission },
    optShow: { ...optShow },
    requiredQuery: ['areaId'],
    crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.artifact-tree',
  paginate: true,
  extraHeight: 40
})

function dbclick(row, column, event) {
  if (column.property === 'remark' && !row.edit && row.dataType === 2) {
    row.edit = true
  }
}

function cancelIt(row) {
  row.remark = row.originalRemark
  row.edit = false
}

async function saveIt(row) {
  try {
    row.editLoading = true
    await editRemark({
      id: row.id,
      remark: row.remark
    })
    this.$notify({ title: '修改成功', type: 'success', duration: 2500 })
    crud.toQuery()
  } catch (error) {
    row.remark = row.originalRemark
    console.log('编辑备注', error)
  } finally {
    row.edit = false
    row.editLoading = false
  }
}

function changeIndex(val) {
  if (val.children) {
    return val.index
  } else {
    return val.childIndex
  }
}

function handleNum(row) {
  currentRow.value = row
  numVisible.value = true
}

function viewState(row) {
  currentRow.value = row
  stateVisible.value = true
}

function handleRowClassName({ row, rowIndex }) {
  if (row.abnormal === 1) {
    return 'abnormal-row'
  } else {
    return row.children ? 'parent-row' : 'hidden-select'
  }
}

function cellClassName({ row, rowIndex }) {
  if (row.abnormal === 1) {
    return 'abnormal-row'
  } else {
    return row.children ? 'parent-row' : 'hidden-select'
  }
}

CRUD.HOOK.handleRefresh = (crud, data) => {
  let index = 1
  data.data.content = data.data.content.map((v) => {
    v.edit = false
    v.originalRemark = v.remark
    v.editLoading = false
    v.monomerId = crud.query.monomerId
    v.areaId = crud.query.areaId
    v.dataType = 2
    v.index = index
    index++
    v.machinePartDTOList = []
    v.children = []
    v.rowKey = v.id
    v.hasChildren = !!v.hasMachinePart
    v.productType = componentTypeEnum.ARTIFACT.V
    v.rowKey = v.id
    return v
  })
}

async function changeStatus(data, val) {
  try {
    const messageName = val ? '启用' : '暂停'
    await ElMessageBox.confirm('确定' + messageName + '?', '提示', {
      confirmButtonText: '确定',
      cancelButtonText: '取消',
      type: 'warning'
    })
    const submitData = {
      status: val
    }
    await editStatus(data.dataType, data.id, submitData)
    crud.notify(`“${data.serialNumber}”已【${messageName}】`, CRUD.NOTIFICATION_TYPE.SUCCESS)
  } catch (error) {
    console.log('修改零构件状态', error)
    data.boolStatusEnum = !data.boolStatusEnum
  }
}

CRUD.HOOK.beforeSubmit = () => {
  crud.form.projectId = globalProjectId
  return !!crud.form.projectId
}

async function load({ row, treeNode, resolve }) {
  try {
    const { content } = await artifactPart({ artifactId: row.id })
    let childIndex = 1
    if (content.length > 0) {
      content.map((v) => {
        v.dataType = 1
        v.rowKey = `${row.id}__${v.id}`
        v.childIndex = childIndex
        childIndex++
        v.productType = componentTypeEnum.MACHINE_PART.V
        return v
      })
    }
    row.machinePartDTOList = content
    resolve(content)
    // 解决lazy首次异步加载数据完成后不展开
    nextTick(() => {
      tableRef.value.toggleRowExpansion(row, true)
    })
  } catch (error) {
    console.log('获取零件信息', error)
  }
}

function getAreaData(val) {
  allArea.value = val
}

function handleSuccess() {
  tableRef.value.refreshParent(currentRow.value)
  crud.toQuery()
}
</script>

<style lang="scss" scoped>
::v-deep(.parent-row) {
  background: #e8f4ff;
}
::v-deep(.abnormal-row) {
  background: #ffecec;
}
::v-deep(.hidden-select) {
  td:nth-child(1) {
    .cell {
      opacity: 0;
    }
  }
}
$font-size: 1.5em;
.child {
  width: $font-size;
  height: $font-size;
  display: inline-block;
  border: 1px solid;
  border-radius: 50%;
  line-height: $font-size;
}
::v-deep(.el-drawer__body) {
  padding-top: 0 !important;
}
</style>
