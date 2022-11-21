<template>
  <div class="app-container">
    <template v-if="pageShow">
      <!--工具栏-->
      <div class="head-container">
        <mHeader :project-id="globalProjectId" :globalProject="globalProject"/>
      </div>
      <!--表格渲染-->
      <common-table
        ref="tableRef"
        v-loading="crud.loading"
        :data="crud.data"
        :empty-text="crud.emptyText"
        :max-height="maxHeight"
        :tree-props="{ children: 'children', hasChildren: 'hasChildren' }"
        row-key="rowKey"
        style="width: 100%"
        :stripe="false"
        lazy
        :load="load"
        return-source-data
        @sort-change="crud.handleSortChange"
        @selection-change="crud.selectionChangeHandler"
        :showEmptySymbol="false"
      >
        <el-table-column key="selection" type="selection" width="55" />
        <el-table-column prop="index" label="序号" align="center" width="60">
          <template v-slot="scope">
            <span v-if="scope.row.dataType===2">{{ changeIndex(scope.row) }}</span>
            <span v-else class="child">{{ changeIndex(scope.row) }}</span>
          </template>
        </el-table-column>
        <el-table-column v-if="columns.visible('serialNumber')" prop="serialNumber" :show-overflow-tooltip="true" align="center" label="单元编号">
          <template #header>
            <el-tooltip class="item" effect="light" :content="`双击编号可预览图纸`" placement="top">
              <div style="display: inline-block">
                <span>单元编号</span>
                <i class="el-icon-info" />
              </div>
            </el-tooltip>
          </template>
          <template v-slot="scope">
            <span style="cursor: pointer" @dblclick="drawingPreview(scope.row)" v-if="scope.row.dataType===2">{{ scope.row.serialNumber }}</span>
          </template>
        </el-table-column>
        <el-table-column v-if="columns.visible('serialNumber')" prop="serialNumber" :show-overflow-tooltip="true" align="center" label="零件编号">
          <template v-slot="scope">
            <span v-if="scope.row.dataType===1">{{ scope.row.serialNumber }}</span>
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('specification')"
          key="specification"
          prop="specification"
          sortable="custom"
          :show-overflow-tooltip="true"
          label="规格"
          align="center"
          min-width="120"
        >
          <template v-slot="scope">
            {{ scope.row.specification ? scope.row.specification : '-' }}
          </template>
        </el-table-column>
         <el-table-column
          v-if="columns.visible('thickness')"
          key="thickness"
          prop="thickness"
          sortable="custom"
          :show-overflow-tooltip="true"
          :label="`厚度\n(mm)`"
          align="left"
          min-width="85px"
        >
          <template v-slot="scope">
            {{ scope.row.thickness ? scope.row.thickness.toFixed(DP.MES_ARTIFACT_T__MM) : '-' }}
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('width')"
          key="width"
          prop="width"
          sortable="custom"
          :show-overflow-tooltip="true"
          :label="`宽度\n(mm)`"
          align="left"
          min-width="85px"
        >
          <template v-slot="scope">
            {{ scope.row.width ? scope.row.width.toFixed(DP.MES_ARTIFACT_L__MM) : '-' }}
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
          align="center"
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
          :label="`单净重(kg)`"
          align="center"
          min-width="80px"
        >
          <template v-slot="scope">
            {{ scope.row.netWeight? scope.row.netWeight.toFixed(DP.COM_WT__KG) : '-' }}
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('totalNetWeight')"
          key="totalNetWeight"
          prop="totalNetWeight"
          :show-overflow-tooltip="true"
          :label="`总净重(kg)`"
          align="center"
          min-width="95px"
        >
          <template v-slot="scope">
            {{ scope.row.totalNetWeight ? scope.row.totalNetWeight.toFixed(DP.COM_WT__KG) : '-' }}
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
          v-if="columns.visible('remark')"
          key="remark"
          prop="remark"
          :show-overflow-tooltip="true"
          label="备注"
          align="center"
          min-width="100"
        >
         <template v-slot="scope">
            {{ scope.row.remark ? scope.row.remark : '-' }}
          </template>
        </el-table-column>
      </common-table>
      <!--分页组件-->
      <pagination />
      <!-- pdf预览 -->
      <drawing-preview-fullscreen-dialog
        v-model="showDrawing"
        :bool-bim="drawingRow?.boolBim"
        :serial-number="drawingRow?.serialNumber"
        :productId="drawingRow?.productId"
        :productType="drawingRow?.productType"
        :projectType="drawingRow?.projectType"
      />
    </template>
    <template v-else>
      <span style="color:red;font-size:13px;">{{pageText}}</span>
    </template>
  </div>
</template>

<script setup>
import crudApi, { cellPartDetail } from '@/api/bridge/bridge-plan/cell-part'
import { ref, watch, nextTick } from 'vue'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import { mapGetters } from '@/store/lib'
import { DP } from '@/settings/config'
import { assemblyListPM as permission } from '@/page-permission/plan'
import useDrawing from '@compos/use-drawing'
import { TechnologyTypeAllEnum, projectTypeEnum } from '@enum-ms/contract'
import { bridgeDeepenTypeEnum } from '@enum-ms/plan'

import pagination from '@crud/Pagination'
import mHeader from './module/header'
import drawingPreviewFullscreenDialog from '@comp-base/drawing-preview/drawing-preview-fullscreen-dialog'

const { globalProject, globalProjectId } = mapGetters(['globalProject', 'globalProjectId'])
const { showDrawing, drawingRow, drawingPreview } = useDrawing({ pidField: 'id' })

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const pageShow = ref(false)
const pageText = ref()

const { crud, columns, CRUD } = useCRUD(
  {
    title: '单元零件清单',
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
  wrapperBox: '.box-list',
  paginate: true,
  extraHeight: 40
})

watch(
  () => globalProjectId.value,
  (val) => {
    if (val) {
      crud.toQuery()
    }
  },
  { immediate: true, deep: true }
)

watch(
  () => globalProject.value,
  (val) => {
    if (!globalProject.value?.projectContentList?.length || globalProject.value?.projectContentList.findIndex(v => v.no === TechnologyTypeAllEnum.BRIDGE.V) < 0) {
      pageText.value = '当前项目内容没有包含分段,请到合同管理中进行配置'
      pageShow.value = false
    } else {
      pageText.value = ''
      pageShow.value = true
    }
  },
  { deep: true, immediate: true }
)

function changeIndex(val) {
  if (val.dataType === 2) {
    return val.index
  } else {
    return val.childIndex
  }
}

CRUD.HOOK.handleRefresh = (crud, data) => {
  let index = 1
  data.data.content = data.data.content.map((v) => {
    v.dataType = 2
    v.index = index
    index++
    v.rowKey = v.id
    v.hasChildren = true
    v.productType = bridgeDeepenTypeEnum.CELL.V
    v.projectType = projectTypeEnum.BRIDGE.V
    return v
  })
}

async function load({ row, treeNode, resolve }) {
  try {
    const { content } = await cellPartDetail({ elementId: row.id })
    let childIndex = 1
    if (content.length > 0) {
      content.map((v) => {
        v.dataType = 1
        v.rowKey = `${row.id}__${v.id}`
        v.childIndex = childIndex
        childIndex++
        v.productType = bridgeDeepenTypeEnum.part.V
        v.projectType = projectTypeEnum.BRIDGE.V
        return v
      })
    }
    resolve(content)
    // 解决lazy首次异步加载数据完成后不展开
    nextTick(() => {
      tableRef.value.toggleRowExpansion(row, true)
    })
  } catch (error) {
    console.log('获取零件信息', error)
  }
}

</script>

<style lang="scss" scoped>
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
