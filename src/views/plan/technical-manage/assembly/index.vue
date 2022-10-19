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
        :showEmptySymbol="false"
      >
        <el-table-column prop="index" label="序号" align="center" width="60">
          <template v-slot="scope">
            <span v-if="scope.row.dataType===2">{{ changeIndex(scope.row) }}</span>
            <span v-else class="child">{{ changeIndex(scope.row) }}</span>
          </template>
        </el-table-column>
        <el-table-column key="productionLineTypeEnum" prop="productionLineTypeEnum" align="center" :show-overflow-tooltip="true" label="生产线" width="80">
          <template v-slot="scope">
            <span>{{ scope.row.productionLineTypeEnum && scope.row.dataType===2? artifactProductLineEnum.VL[scope.row.productionLineTypeEnum] : '' }}</span>
          </template>
        </el-table-column>
        <el-table-column v-if="columns.visible('serialNumber')" prop="serialNumber" :show-overflow-tooltip="true" align="center" label="部件号">
          <template #header>
            <el-tooltip class="item" effect="light" :content="`双击编号可预览图纸`" placement="top">
              <div style="display: inline-block">
                <span>部件号</span>
                <i class="el-icon-info" />
              </div>
            </el-tooltip>
          </template>
          <template v-slot="scope">
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
          align="center"
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
          align="center"
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
          :label="`单重(kg)`"
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
          :label="`总重(kg)`"
          align="center"
          min-width="95px"
        >
          <template v-slot="scope">
            {{ scope.row.totalNetWeight ? scope.row.totalNetWeight.toFixed(DP.COM_WT__KG) : '-' }}
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('drawingNumber')"
          key="drawingNumber"
          prop="drawingNumber"
          :show-overflow-tooltip="true"
          align="center"
          label="图号"
          min-width="80px"
        >
          <template v-slot="scope">
            {{ scope.row.drawingNumber ? scope.row.drawingNumber : '-' }}
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
        <el-table-column
          v-if="columns.visible('userName')"
          key="userName"
          prop="userName"
          :show-overflow-tooltip="true"
          label="导入人"
          align="center"
          min-width="110"
        >
          <template v-slot="scope">
            {{ scope.row.userName ? scope.row.userName : '-' }}
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
      />
    </template>
    <template v-else>
      <span style="color:red;font-size:13px;">{{pageText}}</span>
    </template>
  </div>
</template>

<script setup>
import crudApi from '@/api/plan/technical-manage/assembly'
import { ref, watch, nextTick } from 'vue'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import { artifactProductLineEnum } from '@enum-ms/mes'
import { mapGetters } from '@/store/lib'
import { DP } from '@/settings/config'
import { assemblyListPM as permission } from '@/page-permission/plan'
import useDrawing from '@compos/use-drawing'
import { TechnologyTypeAllEnum, projectModeEnum } from '@enum-ms/contract'

import pagination from '@crud/Pagination'
import mHeader from './module/header'
import drawingPreviewFullscreenDialog from '@comp-base/drawing-preview/drawing-preview-fullscreen-dialog'

const { globalProject, globalProjectId } = mapGetters(['globalProject', 'globalProjectId'])
const { showDrawing, drawingRow, drawingPreview } = useDrawing({ pidField: 'id', productTypeField: 'ASSEMBLE' })

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
    title: '部件清单',
    sort: ['id.asc'],
    permission: { ...permission },
    optShow: { ...optShow },
    requiredQuery: ['areaId', 'monomerId'],
    crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.assembly',
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

watch(
  () => globalProject.value,
  (val) => {
    if (globalProject.value.projectContentList?.length > 0) {
      if (globalProject.value.projectContentList.findIndex(v => v.no === TechnologyTypeAllEnum.STRUCTURE.V) > -1) {
        pageShow.value = globalProject.value.mode !== projectModeEnum.STRUCTURE.V
        pageText.value = globalProject.value.mode !== projectModeEnum.STRUCTURE.V ? '当前项目内容没有包含构件,请到合同管理中进行配置' : '当前项目模式不包含组立'
      } else {
        pageText.value = '当前项目内容没有包含构件,请到合同管理中进行配置'
        pageShow.value = false
      }
    } else {
      pageText.value = '当前项目内容没有包含构件,请到合同管理中进行配置'
      pageShow.value = false
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
    v.hasChildren = !!v.detailDTOList.length
    console.log(v)
    return v
  })
}

async function load({ row, treeNode, resolve }) {
  try {
    const content = row.detailDTOList
    let childIndex = 1
    if (content.length > 0) {
      content.map((v) => {
        v.dataType = 1
        v.rowKey = `${row.id}__${v.id}`
        v.childIndex = childIndex
        childIndex++
        return v
      })
    }
    resolve(content)
    // 解决lazy首次异步加载数据完成后不展开
    nextTick(() => {
      tableRef.value.toggleRowExpansion(row, true)
    })
  } catch (error) {
    console.log('获取翼腹板信息', error)
  }
}

CRUD.HOOK.beforeSubmit = () => {
  return !!crud.form.monomerId
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
