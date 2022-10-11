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
        class="upload-table"
        return-source-data
        :showEmptySymbol="false"
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
          align="center"
        >
          <template v-slot="scope">
            <span>{{ scope.row.name? scope.row.name: '-' }}</span>
            <div v-if="checkPermission([...permission.edit,...permission.del])">
              <udOperation
                :data="scope.row"
                :permission="permission"
              />
            </div>
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
          v-if="columns.visible('content')"
          key="content"
          prop="content"
          label="项目内容"
          align="center"
        >
          <template v-slot="scope">
            <template v-if="scope.row.monomerDetailList.length > 0">
              <div v-for="(k,i) in scope.row.monomerDetailList" :key="k.type">
                <div :class="i===scope.row.monomerDetailList.length-1?'sandwich-cell-bottom':'sandwich-cell-top'">
                  {{k.label}}
                </div>
              </div>
            </template>
            <div v-else class="sandwich-cell-bottom"></div>
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('unit')"
          key="unit"
          prop="unit"
          label="单位"
          align="center"
        >
          <template v-slot="scope">
            <template v-if="scope.row.monomerDetailList.length > 0">
              <div v-for="(k,i) in scope.row.monomerDetailList" :key="k.type">
                <div :class="i===scope.row.monomerDetailList.length-1?'sandwich-cell-bottom':'sandwich-cell-top'">
                  {{k.unit}}
                </div>
              </div>
            </template>
            <div v-else class="sandwich-cell-bottom"></div>
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('mete')"
          key="mete"
          prop="mete"
          label="工程量"
          align="center"
        >
          <template v-slot="scope">
            <template v-if="scope.row.monomerDetailList.length > 0">
              <div v-for="(k,i) in scope.row.monomerDetailList" :key="k.type">
                <div :class="i===scope.row.monomerDetailList.length-1?'sandwich-cell-bottom':'sandwich-cell-top'">
                  <el-input-number
                    v-if="scope.row.monomerDetailList[i].isModify"
                    v-model="scope.row.monomerDetailList[i].mete"
                    :step="10"
                    :min="0"
                    :max="999999999999"
                    :precision="scope.row.monomerDetailList[i].type===TechnologyTypeAllEnum.STRUCTURE.V?DP.COM_WT__KG:DP.MES_ENCLOSURE_L__M"
                    controls-position="right"
                  />
                  <span v-else>{{ k.type===TechnologyTypeAllEnum.STRUCTURE.V?k.mete.toFixed(DP.COM_WT__KG):k.mete.toFixed(DP.MES_ENCLOSURE_L__M) }}</span>
                </div>
              </div>
            </template>
            <div v-else class="sandwich-cell-bottom"></div>
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('startDate')"
          key="startDate"
          prop="startDate"
          label="开始时间"
          align="center"
        >
          <template v-slot="scope">
            <template v-if="scope.row.monomerDetailList.length > 0">
              <div v-for="(k,i) in scope.row.monomerDetailList" :key="k.type">
                <div :class="i===scope.row.monomerDetailList.length-1?'sandwich-cell-bottom':'sandwich-cell-top'">
                  {{ globalProject.startDate?parseTime(globalProject.startDate,'{y}-{m}-{d}'):'-' }}
                </div>
              </div>
            </template>
            <div v-else class="sandwich-cell-bottom"></div>
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('startDate')"
          key="startDate"
          prop="startDate"
          label="计划完成时间"
          align="center"
        >
          <template v-slot="scope">
            <template v-if="scope.row.monomerDetailList.length > 0">
              <div v-for="(k,i) in scope.row.monomerDetailList" :key="k.type">
                <div :class="i===scope.row.monomerDetailList.length-1?'sandwich-cell-bottom':'sandwich-cell-top'">
                  <el-date-picker
                    v-if="scope.row.monomerDetailList[i].isModify"
                    v-model="scope.row.monomerDetailList[i].date"
                    type="date"
                    value-format="x"
                    placeholder="选择完成日期"
                    :disabledDate="(date) => {if (scope.row.date) { return date.getTime()-1 * 24 * 60 * 60 * 1000 < globalProject.startDate || date.getTime() > scope.row.date } else { return date.getTime()-1 * 24 * 60 * 60 * 1000 < globalProject.startDate}}"
                  />
                  <span>{{ k.date?parseTime(k.date,'{y}-{m}-{d}'):'-' }}</span>
                </div>
              </div>
            </template>
            <div v-else class="sandwich-cell-bottom"></div>
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('dateNum')"
          key="dateNum"
          prop="dateNum"
          label="工期(天)"
          align="center"
        >
          <template v-slot="scope">
            <template v-if="scope.row.monomerDetailList.length > 0">
              <div v-for="(k,i) in scope.row.monomerDetailList" :key="k.type">
                <div :class="i===scope.row.monomerDetailList.length-1?'sandwich-cell-bottom':'sandwich-cell-top'">
                  {{ globalProject.startDate && k.date?dateDifference(globalProject.startDate,k.date):'-' }}
                </div>
              </div>
            </template>
            <div v-else class="sandwich-cell-bottom"></div>
          </template>
        </el-table-column>
        <!--单产品类型编辑与删除-->
       <el-table-column
          label="操作"
          width="160px"
          align="center"
          fixed="right"
        >
          <template v-slot="scope">
            <template v-if="scope.row.monomerDetailList.length > 0">
              <div v-for="(k,i) in scope.row.monomerDetailList" :key="k.type">
                <div :class="i===scope.row.monomerDetailList.length-1?'sandwich-cell-bottom':'sandwich-cell-top'">
                  <template v-if="k.isModify">
                    <common-button type="info" size="mini" @click="rowCancel(scope.row,i)">取消</common-button>
                    <common-button type="primary" size="mini" @click="rowSubmit(scope.row,i)">保存</common-button>
                  </template>
                  <template v-else>
                    <common-button size="mini" @click="handleRow(scope.row,i)" icon="el-icon-edit" type="primary" v-permission="permission.productTypeEdit"/>
                    <common-button size="mini" @click="handleDelete(scope.row,k)" icon="el-icon-delete" type="danger" v-permission="permission.productTypeDel"/>
                  </template>
                </div>
              </div>
            </template>
            <div v-else class="sandwich-cell-bottom"></div>
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
import { monomerListPM as permission } from '@/page-permission/plan'
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
import { parseTime, dateDifference } from '@/utils/date'
import { ElMessage } from 'element-plus'

const { globalProject, globalProjectId } = mapGetters(['globalProject', 'globalProjectId'])

const optShow = {
  add: true,
  edit: false,
  del: true,
  download: false
}

const tableRef = ref()
const originDetailRow = ref({})
const techOptions = [
  { label: '构件', key: 'mainStructure', dateKey: 'mainStructureDate', no: TechnologyTypeAllEnum.STRUCTURE.V, alias: 'STRUCTURE', unit: 't' },
  { label: '箱体', key: 'bridgeStructure', dateKey: 'bridgeStructureDate', no: TechnologyTypeAllEnum.BRIDGE.V, alias: 'STRUCTURE', unit: 't' },
  {
    label: '夹芯板',
    key: 'battenBoard',
    dateKey: 'battenBoardDate',
    no: TechnologyTypeAllEnum.SANDWICH_BOARD.V,
    alias: 'ENCLOSURE',
    unit: 'm'
  },
  {
    label: '压型彩板',
    key: 'contourPlate',
    dateKey: 'contourPlateDate',
    no: TechnologyTypeAllEnum.PROFILED_PLATE.V,
    alias: 'ENCLOSURE',
    unit: 'm'
  },
  {
    label: '折边件',
    key: 'flangingPiece',
    dateKey: 'flangingPieceDate',
    no: TechnologyTypeAllEnum.BENDING.V,
    alias: 'ENCLOSURE',
    unit: 'm'
  },
  {
    label: '桁架楼承板',
    key: 'trussFloorPlate',
    dateKey: 'trussFloorPlateDate',
    no: TechnologyTypeAllEnum.TRUSS_FLOOR_PLATE.V,
    alias: 'ENCLOSURE',
    unit: 'm'
  },
  {
    label: '压型楼承板',
    key: 'pressureBearingPlate',
    dateKey: 'pressureBearingPlateDate',
    no: TechnologyTypeAllEnum.PRESSURE_BEARING_PLATE.V,
    alias: 'ENCLOSURE',
    unit: 'm'
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
  () => globalProjectId.value,
  (val) => {
    if (val) {
      crud.query.projectId = globalProjectId.value
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
          choseVal.label = val.label
          choseVal.unit = val.unit
          choseVal.date = String(choseVal.date)
          choseVal.key = val.key
          choseVal.dateKey = val.key + 'Date'
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

function rowCancel(row, index) {
  row.monomerDetailList[index] = Object.assign(row.monomerDetailList[index], JSON.parse(JSON.stringify(originDetailRow.value)))
  row.monomerDetailList[index].isModify = false
}

async function rowSubmit(row, index) {
  if (!row.monomerDetailList[index].mete) {
    ElMessage.error('工程量必填')
    return
  }
  if (!row.monomerDetailList[index].date) {
    ElMessage.error('计划完成时间必填')
    return
  }
  try {
    row.detailSaveDTOParamList = []
    row.detailSaveDTOParamList = JSON.parse(JSON.stringify(row.monomerDetailList))
    await crudApi.edit(row)
    crud.notify(`修改成功`, CRUD.NOTIFICATION_TYPE.SUCCESS)
    row[row.monomerDetailList[index].key] = row.monomerDetailList[index].mete
    row[row.monomerDetailList[index].dateKey] = row.monomerDetailList[index].date
    row.monomerDetailList[index].isModify = false
  } catch (e) {
    console.log(`修改`, e)
  }
}
function handleRow(row, index) {
  originDetailRow.value = JSON.parse(JSON.stringify(row.monomerDetailList[index]))
  row.monomerDetailList[index].isModify = true
}

async function handleDelete(row, index) {
  try {
    row.detailSaveDTOParamList = []
    row.detailSaveDTOParamList = JSON.parse(JSON.stringify(row.monomerDetailList))
    row.detailSaveDTOParamList.splice(index, 1)
    await crudApi.edit(row)
    crud.notify(`删除成功`, CRUD.NOTIFICATION_TYPE.SUCCESS)
    crud.toQuery()
  } catch (e) {
    console.log(`修改`, e)
  }
}
</script>
<style lang="scss" scoped>
.sandwich-cell-top {
  border-bottom: 1px solid #dfe6ec;
}
.sandwich-cell-top,
.sandwich-cell-bottom {
  padding: 5px;
  height: 40px;
  line-height: 30px;
  box-sizing: border-box;
  overflow: hidden;
  // ::v-deep(.el-input__inner) {
  //   padding: 0;
  //   padding-left: 2px;
  // }
}
.upload-table {
  ::v-deep(.cell) {
    padding-left: 0;
    padding-right: 0;
  }
  ::v-deep(thead.is-group th) {
    background: #fff;
  }
}
::v-deep(.el-table--small .el-table__cell){
  padding:4px 0;
}
</style>
