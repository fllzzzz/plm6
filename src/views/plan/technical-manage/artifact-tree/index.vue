<template>
  <div class="app-container">
    <template v-if="globalProject && globalProject.projectContentList && globalProject.projectContentList.length > 0">
      <!--工具栏-->
      <div class="head-container">
        <mHeader :project-id="globalProjectId" @getAreaData="getAreaData" />
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
        row-key="id"
        style="width: 100%"
        :stripe="false"
        lazy
        :load="load"
        @sort-change="crud.handleSortChange"
        @selection-change="crud.selectionChangeHandler"
        return-source-data
        :showEmptySymbol="false"
      >
        <el-table-column key="selection" type="selection" width="55" />
        <el-table-column prop="index" label="序号" align="center" width="60">
          <template v-slot="scope">
            <span v-if="scope.row.children">{{ changeIndex(scope.row) }}</span>
            <span v-else class="child">{{ changeIndex(scope.row) }}</span>
          </template>
        </el-table-column>
        <el-table-column
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
        </el-table-column>
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
                <span>编号</span>
                <i class="el-icon-info" />
              </div>
            </el-tooltip>
          </template>
          <template v-slot="scope">
            <!-- <span>{{ scope.row.serialNumber }}</span> -->
            <span style="cursor: pointer" @dblclick="drawingPreview(scope.row)">{{ scope.row.serialNumber }}</span>
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
        />
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
        />
        <el-table-column
          v-if="columns.visible('quantity')"
          key="quantity"
          prop="quantity"
          sortable="custom"
          label="数量"
          align="left"
          min-width="80px"
        />
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
          v-if="columns.visible('drawingNumber')"
          key="drawingNumber"
          prop="drawingNumber"
          sortable="custom"
          :show-overflow-tooltip="true"
          label="图号"
          min-width="100px"
        />
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
          v-if="columns.visible('remark')"
          key="remark"
          prop="remark"
          :show-overflow-tooltip="true"
          label="备注"
          min-width="120"
        />
        <el-table-column
          v-if="columns.visible('userName')"
          key="userName"
          prop="userName"
          :show-overflow-tooltip="true"
          label="上传人"
          min-width="110"
        />
        <el-table-column v-if="columns.visible('createTime')" key="createTime" prop="createTime" label="上传时间" min-width="160px">
          <template v-slot="scope">
            <div>{{ scope.row.createTime ? parseTime(scope.row.createTime, '{y}-{m}-{d}') : '-' }}</div>
          </template>
        </el-table-column>
        <el-table-column v-if="columns.visible('status')" key="status" prop="status" label="状态" align="center" width="80px" fixed="right">
          <!-- <template slot="header">
          <el-tooltip
            class="item"
            effect="light"
            :content="`零构件进行与暂停: \n
          1.无论有无生产均可以执行暂停；\n
          2.暂停后，无法扫码上传。\n`"
            placement="top"
          >
            <div style="display:inline-block;">
              <span>状态</span>
              <i class="el-icon-info" />
            </div>
          </el-tooltip>
        </template> -->
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
          label="操作"
          width="220px"
          align="center"
          fixed="right"
        >
          <template v-slot="scope">
            <template v-if="scope.row.dataType === 2">
              <el-tooltip class="item" effect="dark" content="数量更改" placement="top">
                <common-button size="mini" @click="handleNum(scope.row)" v-permission="permission.editNum"><svg-icon icon-class="document" /></common-button>
              </el-tooltip>
              <el-tooltip class="item" effect="dark" content="信息修改" placement="top">
                <common-button size="mini" @click="handleList(scope.row)" icon="el-icon-edit" type="primary" v-permission="permission.editInfo"/>
              </el-tooltip>
              <el-tooltip class="item" effect="dark" content="编号更改" placement="top">
                <common-button size="mini" @click="handleSerial(scope.row)" type="success"><svg-icon icon-class="expand" v-permission="permission.editSerialNum"/></common-button>
              </el-tooltip>
            </template>
          </template>
        </el-table-column>
      </common-table>
      <!--分页组件-->
      <pagination />
      <numForm v-model="numVisible" :detailInfo="currentRow" @success="handleSuccess" />
      <listForm v-model="listVisible" :detailInfo="currentRow" @success="handleSuccess" :allArea="allArea" />
      <serialNumForm v-model="serialVisible" :detailInfo="currentRow" @success="handleSuccess" @numSuccess="handleNumSuccess" :allArea="allArea" />
      <!-- pdf预览 -->
      <bim-preview-drawer
        v-model:visible="showBimDialog"
        :bool-bim="drawingRow?.boolBim"
        :monomer-id="drawingRow?.monomerId"
        :serial-number="drawingRow?.serialNumber"
        :productId="drawingRow?.productId"
        :productType="drawingRow?.productType"
      />
      <!-- pdf预览 -->
      <drawing-preview-fullscreen-dialog
        v-model="showDrawingDialog"
        :bool-bim="drawingRow?.boolBim"
        :serial-number="drawingRow?.serialNumber"
        :productId="drawingRow?.productId"
        :productType="drawingRow?.productType"
      />
    </template>
  </div>
</template>

<script setup>
import crudApi, { editStatus, artifactPart } from '@/api/plan/technical-manage/artifact-tree'
import { ref, nextTick, watch } from 'vue'
import { artifactTreePM as permission } from '@/page-permission/plan'
import checkPermission from '@/utils/system/check-permission'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import useDrawing from '@compos/use-drawing'
import pagination from '@crud/Pagination'
import { mapGetters } from '@/store/lib'
import mHeader from './module/header'
import { DP } from '@/settings/config'
import { ElMessageBox } from 'element-plus'
import { parseTime } from '@/utils/date'
import numForm from './module/num-form'
import listForm from './module/list-form'
import serialNumForm from './module/serialNum-form'
import bimPreviewDrawer from '@/components-system/bim/bim-preview-drawer'
import drawingPreviewFullscreenDialog from '@comp-base/drawing-preview/drawing-preview-fullscreen-dialog'
import { componentTypeEnum } from '@enum-ms/mes'

const { globalProject, globalProjectId } = mapGetters(['globalProject', 'globalProjectId'])
const { showDrawing, drawingRow, drawingPreview } = useDrawing({ pidField: 'id', typeField: 'productType' })

const showBimDialog = ref(false)
const showDrawingDialog = ref(false)

watch(
  [() => showBimDialog.value, () => showDrawingDialog.value],
  ([b, d]) => {
    if (!b && !d) {
      showDrawing.value = false
    }
    console.log(b, d, showDrawing.value, 'show')
  }
)

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

const optShow = {
  add: false,
  edit: false,
  del: true,
  download: false
}

const tableRef = ref()
const currentRow = ref({})
const numVisible = ref(false)
const listVisible = ref(false)
const serialVisible = ref(false)
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

function handleList(row) {
  currentRow.value = row
  listVisible.value = true
}

function handleSerial(row) {
  currentRow.value = row
  serialVisible.value = true
}

function handleRowClassName({ row, rowIndex }) {
  if (row.abnormal === 1) {
    return 'abnormal-row'
  } else {
    return row.children ? 'parent-row' : 'hidden-select'
  }
}

function cellClassName() {
  return ''
}

CRUD.HOOK.handleRefresh = (crud, data) => {
  let index = 1
  data.data.content = data.data.content.map((v) => {
    v.monomerId = crud.query.monomerId
    v.areaId = crud.query.areaId
    v.dataType = 2
    v.index = index
    index++
    v.machinePartDTOList = []
    v.children = []
    v.hasChildren = !!v.hasMachinePart
    v.productType = componentTypeEnum.ARTIFACT.V
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

function handleNumSuccess() {
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
