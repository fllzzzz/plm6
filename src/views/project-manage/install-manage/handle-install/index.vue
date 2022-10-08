<template>
  <div class="app-container">
    <div v-if="globalProject?.businessType===businessTypeEnum.INSTALLATION.V">
      <mHeader :projectId="globalProjectId" :globalProject="globalProject"/>
      <!--表格渲染-->
      <common-table
        ref="tableRef"
        v-loading="crud.loading"
        :data="crud.data"
        :data-format="dataFormat"
        :empty-text="crud.emptyText"
        :max-height="maxHeight"
        style="width: 100%;margin-top:10px;"
        :stripe="false"
      >
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column key="monomer.name" prop="monomer.name" label="单体" align="center" show-overflow-tooltip />
        <el-table-column key="area.name" prop="area.name" label="区域" align="center" show-overflow-tooltip />
        <el-table-column key="name" prop="name" label="名称" align="center" show-overflow-tooltip />
        <el-table-column
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
            <span style="cursor: pointer" @dblclick="drawingPreview(scope.row)">{{ scope.row.serialNumber }}</span>
          </template>
        </el-table-column>
        <el-table-column key="specification" prop="specification" label="规格" align="center" show-overflow-tooltip min-width="120" />
        <el-table-column key="measureUnit" prop="measureUnit" label="计量单位" align="center" show-overflow-tooltip />
        <el-table-column key="quantity" prop="quantity" label="清单数" align="center" show-overflow-tooltip />
        <el-table-column key="accountingUnit" prop="accountingUnit" label="核算单位" align="center" show-overflow-tooltip />
        <el-table-column key="mete" prop="mete" label="清单量" align="center" show-overflow-tooltip />
        <el-table-column key="receivingQuantity" prop="receivingQuantity" label="收货量" align="center" show-overflow-tooltip />
        <el-table-column key="installQuantity" prop="installQuantity" label="已安装量" align="center" show-overflow-tooltip />
        <el-table-column key="unAuditQuantity" prop="unAuditQuantity" label="待审核量" align="center" show-overflow-tooltip />
        <el-table-column key="reportQuantity" prop="reportQuantity" label="本次安装填报" align="center" v-if="globalProject?.installReportMethod === installSetEnum.PC.V">
          <template v-slot="scope">
            <el-input-number
              v-if="scope.row.installAbleNum>0"
              v-model.number="scope.row.sourceRow.reportQuantity"
              :min="0"
              :max="scope.row.installAbleNum"
              :step="1"
              :precision="0"
              placeholder="请填写"
              controls-position="right"
              style="width: 100%"
            />
          </template>
        </el-table-column>
      </common-table>
      <!--分页组件-->
      <el-pagination
        :total="total"
        :current-page="queryPage.pageNumber"
        :page-size="queryPage.pageSize"
        style="margin-top: 8px"
        layout="total, prev, pager, next, sizes"
        @size-change="handleSizeChange"
        @current-change="handleCurrentChange"
      />
      <!-- pdf预览 -->
      <bim-preview-drawer
        v-model:visible="showBimDialog"
        :bool-bim="drawingRow?.boolBim"
        :drawingSN="drawingRow?.drawingSN"
        :monomer-id="drawingRow?.monomerId"
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
    </div>
    <div v-else>
      <el-tag type="danger" size="medium" style="margin-bottom: 10px"> * 您好，请先选择业务类型为项目承包的项目，当前页面需要选择业务类型为项目承包方可查看 </el-tag>
    </div>
  </div>
</template>

<script setup>
import crudApi from '@/api/project-manage/install-manage/handle-install'
import { ref, reactive, watch } from 'vue'

import { businessTypeEnum } from '@enum-ms/contract'
import { handleInstallPM as permission } from '@/page-permission/project'
import { mapGetters } from '@/store/lib'
import { installSetEnum } from '@enum-ms/project'
import { componentTypeEnum } from '@enum-ms/mes'
import useDrawing from '@compos/use-drawing'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import mHeader from './header'
import bimPreviewDrawer from '@/components-system/bim/bim-preview-drawer'
import drawingPreviewFullscreenDialog from '@comp-base/drawing-preview/drawing-preview-fullscreen-dialog'

const { globalProjectId, globalProject } = mapGetters(['globalProjectId', 'globalProject'])
const { showDrawing, drawingRow, drawingPreview } = useDrawing({ pidField: 'productId', typeField: 'productType' })

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const dataFormat = ref([])
const showBimDialog = ref(false)
const showDrawingDialog = ref(false)
const tableRef = ref()

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

const { crud, CRUD } = useCRUD(
  {
    title: '安装填报',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    requiredQuery: ['projectId', 'productType'],
    hasPagination: false
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  paginate: true,
  extraHeight: 40
})

// 分页操作
const queryPage = reactive({
  pageNumber: 1,
  pageSize: 100
})
const total = ref(0)
function handleSizeChange(val) {
  queryPage.pageNumber = 1
  queryPage.pageSize = val
  crud.toQuery()
}
function handleCurrentChange(val) {
  queryPage.pageNumber = val
  crud.toQuery()
}

CRUD.HOOK.beforeRefresh = () => {
  crud.query.pageNumber = queryPage.pageNumber
  crud.query.pageSize = queryPage.pageSize
  crud.query.projectId = globalProject.value.businessType === businessTypeEnum.INSTALLATION.V ? globalProjectId.value : undefined
  return !!crud.query.projectId
}

CRUD.HOOK.handleRefresh = (crud, { data }) => {
  data.content.forEach(v => {
    v.areaId = v.area?.id
    v.monomerId = v.monomer?.id
    v.installAbleNum = v.receivingQuantity ? v.receivingQuantity - (v.installQuantity || 0) - (v.unAuditQuantity || 0) : 0
  })
  total.value = data.totalElements
}

</script>

<style lang="scss" scoped>
.collection-table{
  ::v-deep(.el-select .el-input__inner){
    padding-left:2px;
    padding-right:5px;
  }
  ::v-deep(.el-input-number .el-input__inner, .el-input__inner) {
    text-align: left;
    padding:0 5px;
  }
  ::v-deep(.el-table .cell){
    padding-left:2px;
    padding-right:2px;
  }
}
</style>
