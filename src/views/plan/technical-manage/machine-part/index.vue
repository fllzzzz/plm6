<template>
  <div class="app-container">
    <template v-if="globalProject && globalProject.projectContentList && globalProject.projectContentList.length > 0">
      <!--工具栏-->
      <div class="head-container">
        <mHeader :project-id="globalProjectId" />
      </div>
      <!--表格渲染-->
      <common-table
        ref="tableRef"
        v-loading="crud.loading"
        :data="crud.data"
        :empty-text="crud.emptyText"
        return-source-data
        :showEmptySymbol="false"
        :max-height="maxHeight"
        style="width: 100%"
        @sort-change="crud.handleSortChange"
      >
        <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
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
          <template v-slot:header>
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
          v-if="columns.visible('producedQuantity')"
          key="producedQuantity"
          prop="producedQuantity"
          sortable="custom"
          label="总生产量"
          align="center"
          width="100px"
        />
        <el-table-column
          v-if="columns.visible('usedQuantity')"
          key="usedQuantity"
          prop="usedQuantity"
          sortable="custom"
          label="已使用量"
          align="left"
          min-width="90px"
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
          v-if="columns.visible('remark')"
          key="remark"
          prop="remark"
          :show-overflow-tooltip="true"
          label="备注"
          min-width="120"
        />
      </common-table>
      <!--分页组件-->
      <pagination />
    </template>
    <!-- <template v-else>
      <div style="color:red;font-size:14px;">*请先前去合同管理模块添加项目内容</div>
    </template> -->
    <!-- pdf预览 -->
    <drawing-preview-fullscreen-dialog
        v-model="showDrawing"
        :bool-bim="drawingRow?.boolBim"
        :serial-number="drawingRow?.serialNumber"
        :productId="drawingRow?.productId"
        :productType="drawingRow?.productType"
      />
  </div>
</template>

<script setup>
import crudApi from '@/api/plan/technical-manage/machine-part'
import { ref, watch } from 'vue'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import useDrawing from '@compos/use-drawing'
import pagination from '@crud/Pagination'
import { mapGetters } from '@/store/lib'
import mHeader from './module/header'
import { DP } from '@/settings/config'
import { machinePartPM as permission } from '@/page-permission/plan'
import drawingPreviewFullscreenDialog from '@comp-base/drawing-preview/drawing-preview-fullscreen-dialog'

const { globalProject, globalProjectId } = mapGetters(['globalProject', 'globalProjectId'])
const { showDrawing, drawingRow, drawingPreview } = useDrawing({ pidField: 'id', productTypeField: 'MACHINE_PART' })

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const { crud, columns } = useCRUD(
  {
    title: '零件清单',
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
  wrapperBox: '.machine-part',
  paginate: true,
  extraHeight: 40
})

watch(
  () => globalProjectId.value,
  (val) => {
    if (val) {
      crud.query.projectId = globalProjectId.value
      crud.toQuery()
    }
  },
  { immediate: true, deep: true }
)
</script>

<style lang="scss" scoped>
::v-deep(.abnormal-row) {
  background: #e8f4ff;
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
</style>
