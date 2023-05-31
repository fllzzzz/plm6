<template>
  <div class="app-container">
    <!--工具栏-->
    <div class="head-container">
      <mHeader />
    </div>
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      return-source-data
      :showEmptySymbol="false"
      style="width: 100%"
      @selection-change="crud.selectionChangeHandler"
      @sort-change="crud.handleSortChange"
    >
      <el-table-column key="selection" type="selection" width="55" />
      <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
      <el-table-column
        v-if="columns.visible('monomer.name')"
        key="monomer.name"
        prop="monomer.name"
        :show-overflow-tooltip="true"
        label="单体"
        min-width="100px"
      />
      <el-table-column
        v-if="columns.visible('area.name')"
        key="area.name"
        prop="area.name"
        :show-overflow-tooltip="true"
        label="区域"
        min-width="100px"
      />
      <el-table-column v-if="columns.visible('name')" key="name" prop="name" :show-overflow-tooltip="true" label="名称" min-width="100px" />
      <el-table-column
        v-if="columns.visible('serialNumber')"
        key="serialNumber"
        prop="serialNumber"
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
          <span style="cursor: pointer" @dblclick="drawingPreview(scope.row)" class="tc-primary">{{ scope.row.serialNumber }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('oldSpecification')"
        key="oldSpecification"
        prop="oldSpecification"
        :show-overflow-tooltip="true"
        :label="crud.query.boolAmendStatus ? '旧规格' : '规格'"
        min-width="120"
      />
      <el-table-column
        v-if="columns.visible('newSpecification')"
        key="newSpecification"
        prop="newSpecification"
        :show-overflow-tooltip="true"
        label="新规格"
        min-width="120"
      >
        <template v-slot="scope">
          <span>{{ scope.row.newSpecification || '-' }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('length')"
        key="length"
        prop="length"
        :show-overflow-tooltip="true"
        label="长度(mm)"
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
        :show-overflow-tooltip="true"
        label="材质"
        align="left"
        min-width="80px"
      />
      <el-table-column v-if="columns.visible('quantity')" key="quantity" prop="quantity" label="清单数" align="left" min-width="80px" />
      <el-table-column
        v-if="columns.visible('netWeight')"
        key="netWeight"
        prop="netWeight"
        :show-overflow-tooltip="true"
        label="单净重(kg)"
        align="left"
        min-width="80px"
      >
        <template v-slot="scope">
          {{ scope.row.netWeight ? scope.row.netWeight.toFixed(DP.COM_WT__KG) : '-' }}
        </template>
      </el-table-column>
      <!--编辑与删除-->
      <el-table-column
        v-if="crud.query.boolAmendStatus === false && checkPermission([...permission.edit])"
        label="操作"
        width="130px"
        align="center"
        fixed="right"
      >
        <template v-slot="scope">
          <udOperation :data="scope.row" :show-del="false" :show-edit="!scope.row.newSpecPrefix" :permission="permission" />
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
    <mForm />
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
  </div>
</template>

<script setup>
import crudApi, { specConfig, getSpecList } from '@/api/mes/craft-manage/artifact-specification-revise'
import { ref, provide, watch } from 'vue'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import { DP } from '@/settings/config'
import { artifactProductLineEnum, componentTypeEnum } from '@enum-ms/mes'
import { artifactSpecificationRevisePM as permission } from '@/page-permission/mes'
import checkPermission from '@/utils/system/check-permission'
import useDrawing from '@compos/use-drawing'
import udOperation from '@crud/UD.operation'
import bimPreviewDrawer from '@/components-system/bim/bim-preview-drawer'
import drawingPreviewFullscreenDialog from '@comp-base/drawing-preview/drawing-preview-fullscreen-dialog'
import mHeader from './module/header'
import mForm from './module/form'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const specList = ref([])
const specificationList = ref([])
const { crud, CRUD, columns } = useCRUD(
  {
    title: '构件规格修正',
    sort: ['id.asc'],
    permission: { ...permission },
    optShow: { ...optShow },
    requiredQuery: ['projectId'],
    crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.artifact-specification-revise',
  paginate: true,
  extraHeight: 40
})

getSpecConfig()

watch(
  [
    () => crud.query.projectId,
    () => crud.query.boolAmendStatus,
    () => crud.query.monomerId,
    () => crud.query.areaId,
    () => crud.query.serialNumber,
    () => crud.query.name,
    () => crud.query.oldSpecification,
    () => crud.query.newSpecification
  ],
  (val) => {
    crud.query.specification = undefined
    fetchSpec()
  }
)

provide('specList', specList.value)
provide('specificationList', specificationList)

// function selectable(row, rowIndex) {
//   return !row.newSpecPrefix
// }
const { showDrawing, drawingRow, drawingPreview } = useDrawing({ pidField: 'artifactId', typeField: 'productType' })

const showBimDialog = ref(false)
const showDrawingDialog = ref(false)

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

async function getSpecConfig() {
  specList.value = []
  try {
    const { content } = await specConfig({ productionLineTypeEnum: artifactProductLineEnum.TRADITION.V })
    for (let i = 0; i < content.length; i++) {
      specList.value.push({
        id: content[i],
        value: content[i]
      })
    }
  } catch (e) {
    console.log('获取构件截面定义', e)
  }
}

async function fetchSpec() {
  specificationList.value = []
  try {
    const data = await getSpecList({
      boolAmendStatus: crud.query.boolAmendStatus,
      monomerId: crud.query.monomerId,
      areaId: crud.query.areaId,
      projectId: crud.query.projectId,
      oldSpecification: crud.query.oldSpecification,
      newSpecification: crud.query.newSpecification,
      name: crud.query.name,
      serialNumber: crud.query.serialNumber
    })
    for (let i = 0; i < data?.length; i++) {
      specificationList.value.push({
        id: data[i],
        name: data[i]
      })
    }
  } catch (e) {
    console.log('获取规格列表失败', e)
  }
}

CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data?.content.map((v) => {
    v.productType = componentTypeEnum.ARTIFACT.V
    v.monomerId = v.monomer?.id
    return v
  })
}
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
