<template>
  <div class="app-container">
    <template v-if="globalProject && globalProject.projectContentList && globalProject.projectContentList.length > 0">
      <!--工具栏-->
      <div class="head-container">
        <mHeader :globalProject="globalProject"/>
      </div>
      <!--表格渲染-->
      <common-table
        ref="tableRef"
        v-loading="crud.loading"
        :data="crud.data"
        :empty-text="crud.emptyText"
        :max-height="maxHeight"
        style="width: 100%"
        row-key="id"
        @selection-change="crud.selectionChangeHandler"
      >
        <el-table-column
          type="selection"
          width="55"
          align="center"
        />
        <el-table-column
          label="序号"
          type="index"
          align="center"
          width="60"
        />
        <el-table-column
          v-if="columns.visible('name')"
          key="name"
          prop="name"
          :show-overflow-tooltip="true"
          label="单体名称"
        />
        <el-table-column
          v-if="columns.visible('mainStructure')"
          key="mainStructure"
          prop="mainStructure"
          :show-overflow-tooltip="true"
          label="构件(t)"
        >
          <template v-slot="scope">
            <span>{{ scope.row.mainStructure? scope.row.mainStructure.toFixed(DP.COM_WT__KG): '-' }}</span>
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('battenBoard')"
          key="battenBoard"
          prop="battenBoard"
          :show-overflow-tooltip="true"
          label="夹芯板(m)"
        >
          <template v-slot="scope">
            <span>{{ scope.row.battenBoard? scope.row.battenBoard.toFixed(DP.COM_WT__KG): '-' }}</span>
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('contourPlate')"
          key="contourPlate"
          prop="contourPlate"
          :show-overflow-tooltip="true"
          label="压型板(m)"
        >
          <template v-slot="scope">
            <span>{{ scope.row.contourPlate? scope.row.contourPlate.toFixed(DP.COM_WT__KG): '-' }}</span>
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('flangingPiece')"
          key="flangingPiece"
          prop="flangingPiece"
          :show-overflow-tooltip="true"
          label="折边件(m)"
        >
          <template v-slot="scope">
            <span>{{ scope.row.flangingPiece? scope.row.flangingPiece.toFixed(DP.COM_WT__KG): '-' }}</span>
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('trussFloorPlate')"
          key="trussFloorPlate"
          prop="trussFloorPlate"
          :show-overflow-tooltip="true"
          label="桁架楼承板(m)"
        >
          <template v-slot="scope">
            <span>{{ scope.row.trussFloorPlate? scope.row.trussFloorPlate.toFixed(DP.COM_WT__KG): '-' }}</span>
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('pressureBearingPlate')"
          key="pressureBearingPlate"
          prop="pressureBearingPlate"
          :show-overflow-tooltip="true"
          label="压型楼承板(m)"
        >
          <template v-slot="scope">
            <span>{{ scope.row.pressureBearingPlate?scope.row.pressureBearingPlate.toFixed(DP.COM_WT__KG): '-' }}</span>
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('sort')"
          key="sort"
          prop="sort"
          label="排序"
          align="center"
        />
        <el-table-column
          v-if="columns.visible('remark')"
          key="remark"
          prop="remark"
          :show-overflow-tooltip="true"
          label="备注"
        />
        <!--编辑与删除-->
        <el-table-column
          v-if="checkPermission([...permission.edit, ...permission.del])"
          label="操作"
          width="130px"
          align="center"
          fixed="right"
        >
          <template v-slot="scope">
            <udOperation
              :data="scope.row"
              :permission="permission"
            />
          </template>
        </el-table-column>
      </common-table>
      <!--分页组件-->
      <pagination />
      <mForm
        :project-id="globalProjectId"
        :global-project="globalProject"
        :origin-option="techOptions"
      />
    </template>
    <template v-else>
      <div style="color: red; font-size: 14px">*请先前去合同管理模块添加项目内容</div>
    </template>
  </div>
</template>

<script setup>
import crudApi from '@/api/plan/monomer'
import { ref, watch } from 'vue'
import checkPermission from '@/utils/system/check-permission'
import { TechnologyTypeAllEnum } from '@enum-ms/contract'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import udOperation from '@crud/UD.operation'
import pagination from '@crud/Pagination'
import { mapGetters } from '@/store/lib'
import mHeader from './module/header'
import mForm from './module/form'
import { DP } from '@/settings/config'

const { globalProject, globalProjectId } = mapGetters(['globalProject', 'globalProjectId'])
// crud交由presenter持有
const permission = {
  get: ['monomer:get'],
  add: ['monomer:add'],
  edit: ['monomer:edit'],
  del: ['monomer:del']
}

const optShow = {
  add: true,
  edit: false,
  del: true,
  download: false
}

const tableRef = ref()
const techOptions = [
  { label: '构件', key: 'mainStructure', dateKey: 'mainStructureDate', no: TechnologyTypeAllEnum.STRUCTURE.V, alias: 'STRUCTURE', unit: '(t)' },
  {
    label: '夹芯板',
    key: 'battenBoard',
    dateKey: 'battenBoardDate',
    no: TechnologyTypeAllEnum.SANDWICH_BOARD.V,
    alias: 'ENCLOSURE',
    unit: '(m)'
  },
  {
    label: '压型板',
    key: 'contourPlate',
    dateKey: 'contourPlateDate',
    no: TechnologyTypeAllEnum.PROFILED_PLATE.V,
    alias: 'ENCLOSURE',
    unit: '(m)'
  },
  {
    label: '折边件',
    key: 'flangingPiece',
    dateKey: 'flangingPieceDate',
    no: TechnologyTypeAllEnum.BENDING.V,
    alias: 'ENCLOSURE',
    unit: '(m)'
  },
  {
    label: '桁架楼承板',
    key: 'trussFloorPlate',
    dateKey: 'trussFloorPlateDate',
    no: TechnologyTypeAllEnum.TRUSS_FLOOR_PLATE.V,
    alias: 'ENCLOSURE',
    unit: '(m)'
  },
  {
    label: '压型楼承板',
    key: 'pressureBearingPlate',
    dateKey: 'pressureBearingPlateDate',
    no: TechnologyTypeAllEnum.PRESSURE_BEARING_PLATE.V,
    alias: 'ENCLOSURE',
    unit: '(m)'
  }
]

const { crud, columns, CRUD } = useCRUD(
  {
    title: '单体',
    sort: ['sort.asc', 'id.desc'],
    permission: { ...permission },
    optShow: { ...optShow },
    requiredQuery: ['projectId'],
    crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.monomer',
  paginate: true,
  extraHeight: 40
})

watch(
  () => globalProjectId,
  (val) => {
    if (val) {
      crud.query.projectId = globalProjectId
      crud.toQuery()
    }
  },
  { immediate: true, deep: true }
)

CRUD.HOOK.handleRefresh = (crud, data) => {
  data.data.content.forEach((v) => {
    v.date = String(v.date)
    if (v.monomerDetailList.length > 0) {
      techOptions.forEach((val) => {
        const choseVal = v.monomerDetailList.find((k) => k.type === val.no)
        if (choseVal) {
          v[val.key] = choseVal.mete
          v[val.key + 'Date'] = String(choseVal.date)
        } else {
          v[val.key] = undefined
          v[val.key + 'Date'] = undefined
        }
      })
    }
    return v
  })
}
</script>
