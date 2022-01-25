<template>
  <div class="app-container">
    <template v-if="globalProject && globalProject.projectContentList && globalProject.projectContentList.length > 0">
      <!--工具栏-->
      <div class="head-container">
        <mHeader :project-id="globalProjectId" @tableAdd="tableAdd" :table-data="totalTechInfo" :globalProject="globalProject" @categoryChange="getPlate"/>
      </div>
      <!--表格渲染-->
      <common-table
        ref="tableRef"
        v-loading="crud.loading"
        :data="crud.data"
        :empty-text="crud.emptyText"
        :max-height="maxHeight"
        style="width: 100%"
        @sort-change="crud.handleSortChange"
        class="enclosure-table"
        :cell-class-name="wrongCellMask"
      >
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column
          v-if="columns.visible('name')"
          key="name"
          prop="name"
          :show-overflow-tooltip="true"
          label="名称"
          min-width="100"
        >
          <template v-slot="scope">
            <div>{{ scope.row.name  }}</div>
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('serialNumber')"
          key="serialNumber"
          prop="serialNumber"
          :show-overflow-tooltip="true"
          label="编号"
          min-width="90px"
        >
          <!-- <template slot="header">
          <el-tooltip
            class="item"
            effect="light"
            :content="`双击编号可预览图纸`"
            placement="top"
          >
            <div style="display:inline-block;">
              <span>编号</span>
              <i class="el-icon-info" />
            </div>
          </el-tooltip>
        </template> -->
          <template v-slot="scope">
            <div>{{ scope.row.serialNumber  }}</div>
            <!-- <span style="cursor: pointer" @dblclick="drawingPreview(scope.row)">{{ scope.row.serialNumber }}</span> -->
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('plateId') && crud.query.category!==TechnologyTypeAllEnum.BENDING.V"
          key="plateId"
          prop="plateId"
          :show-overflow-tooltip="true"
          label="版型"
          min-width="100px"
        >
          <template v-slot="scope">
            <template v-if="scope.row.isModify && !scope.row.inProductionQuantity">
              <common-select
                v-model="scope.row.plateId"
                :options="plateOption"
                :type="'other'"
                :dataStructure="crud.query.category===TechnologyTypeAllEnum.TRUSS_FLOOR_PLATE.V?trussProp:typeProp"
                size="small"
                placeholder="版型"
                @change="plateChange(scope.row,scope.$index)"
              />
            </template>
            <div v-else>{{ scope.row.plate? scope.row.plate: '-' }}</div>
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('width') && crud.query.category!==TechnologyTypeAllEnum.BENDING.V"
          key="width"
          prop="width"
          :show-overflow-tooltip="true"
          :label="crud.query.category===TechnologyTypeAllEnum.SANDWICH_BOARD.V?'宽度\n(mm)':`有效宽度\n(mm)`"
          min-width="100px"
        >
          <template v-slot="scope">
             <el-input-number
              v-if="scope.row.isModify && (crud.query.category!==TechnologyTypeAllEnum.PROFILED_PLATE.V && crud.query.category!==TechnologyTypeAllEnum.PRESSURE_BEARING_PLATE.V) && !scope.row.inProductionQuantity"
              v-model.number="scope.row.width"
              :min="0"
              :max="99999999999"
              :step="1"
              :precision="DP.MES_ENCLOSURE_W__MM"
              placeholder="有效宽度"
              controls-position="right"
              style="width:100%"
               @change="getTotalData(scope.row)"
            />
            <div v-else>{{ scope.row.width? scope.row.width.toFixed(DP.MES_ENCLOSURE_W__MM): '-' }}</div>
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('unfoldedWidth') && crud.query.category===TechnologyTypeAllEnum.BENDING.V"
          key="unfoldedWidth"
          prop="unfoldedWidth"
          :show-overflow-tooltip="true"
          :label="`展开宽度\n(mm)`"
          min-width="100px"
        >
          <template v-slot="scope">
             <el-input-number
              v-if="scope.row.isModify && !scope.row.inProductionQuantity"
              v-model.number="scope.row.unfoldedWidth"
              :min="0"
              :max="99999999999"
              :step="1"
              :precision="DP.MES_ENCLOSURE_W__MM"
              placeholder="展开宽度"
              controls-position="right"
              style="width:100%"
               @change="getTotalData(scope.row)"
            />
            <div v-else>{{ scope.row.unfoldedWidth? scope.row.unfoldedWidth.toFixed(DP.MES_ENCLOSURE_W__MM): '-' }}</div>
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('thickness') && crud.query.category!==TechnologyTypeAllEnum.TRUSS_FLOOR_PLATE.V"
          key="thickness"
          prop="thickness"
          :show-overflow-tooltip="true"
          :label="`板厚\n(mm)`"
          align="left"
          min-width="100px"
        >
          <template v-slot="scope">
            <el-input-number
              v-if="scope.row.isModify && !scope.row.inProductionQuantity"
              v-model.number="scope.row.thickness"
              :min="0"
              :max="99999999999"
              :step="1"
              :precision="DP.MES_ENCLOSURE_T__MM"
              placeholder="板厚"
              controls-position="right"
              style="width:100%"
            />
            <span v-else>{{ scope.row.thickness ? scope.row.thickness.toFixed(DP.MES_ENCLOSURE_T__MM) : '-' }}</span>
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('length')"
          key="length"
          prop="length"
          :show-overflow-tooltip="true"
          :label="`单长\n(㎜)`"
          align="left"
          min-width="100px"
        >
          <template v-slot="scope">
            <el-input-number
              v-if="scope.row.isModify && !scope.row.inProductionQuantity"
              v-model.number="scope.row.length"
              :min="0"
              :max="99999999999"
              :step="1"
              :precision="DP.MES_ENCLOSURE_L__MM"
              placeholder="单长"
              controls-position="right"
              style="width:100%"
              @change="getTotalData(scope.row)"
            />
            <span v-else>{{ scope.row.length ? scope.row.length.toFixed(DP.MES_ENCLOSURE_L__MM) : '-' }}</span>
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('quantity')"
          key="quantity"
          prop="quantity"
          :label="'数量(张)'"
          align="left"
          min-width="100px"
        >
          <template v-slot="scope">
            <el-input-number
              v-if="scope.row.isModify && !scope.row.inProductionQuantity"
              v-model.number="scope.row.quantity"
              :min="0"
              :max="99999999999"
              :step="1"
              :precision="DP.MES_ENCLOSURE_L__MM"
              placeholder="数量"
              controls-position="right"
              style="width:100%"
              @change="getTotalData(scope.row)"
            />
            <span v-else>{{ scope.row.quantity }}</span>
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('totalArea')"
          key="totalArea"
          prop="totalArea"
          :show-overflow-tooltip="true"
          :label="`总面积(㎡)`"
          align="left"
          min-width="100px"
        >
          <template v-slot="scope">
            {{ scope.row.totalArea ? scope.row.totalArea.toFixed(DP.COM_AREA__M2) : '-' }}
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('totalLength')"
          key="totalLength"
          prop="totalLength"
          :label="`总长度(m)`"
          align="left"
          min-width="100px"
        >
          <template v-slot="scope">
            {{ scope.row.totalLength ? scope.row.totalLength.toFixed(DP.MES_ENCLOSURE_L__M) : '-' }}
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('brand') && (crud.query.category!==TechnologyTypeAllEnum.SANDWICH_BOARD.V && crud.query.category!==TechnologyTypeAllEnum.TRUSS_FLOOR_PLATE.V)"
          key="brand"
          prop="brand"
          :show-overflow-tooltip="true"
          label="品牌"
          width="100px"
        >
          <template v-slot="scope">
            <el-input
              v-if="scope.row.isModify"
              v-model="scope.row.brand"
              placeholder="品牌"
              maxlength="10"
              style="width:100%;"
            />
            <div v-else>{{ scope.row.brand ? scope.row.brand : '-' }}</div>
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('color') && crud.query.category!=TechnologyTypeAllEnum.TRUSS_FLOOR_PLATE.V && crud.query.category!=TechnologyTypeAllEnum.PRESSURE_BEARING_PLATE.V && crud.query.category!=TechnologyTypeAllEnum.SANDWICH_BOARD.V"
          key="color"
          prop="color"
          :show-overflow-tooltip="true"
          label="颜色"
          width="100px"
        >
          <template v-slot="scope">
            <el-input
              v-if="scope.row.isModify"
              v-model="scope.row.color"
              placeholder="颜色"
              maxlength="10"
              style="width:100%;"
            />
            <div v-else>{{ scope.row.color ? scope.row.color : '-' }}</div>
          </template>
        </el-table-column>
        <!--状态、编辑与删除-->
        <el-table-column v-if="columns.visible('status')" key="status" prop="status" label="状态" align="center" width="70px" fixed="right">
          <template v-slot="scope">
            <el-switch
              v-if="scope.row.id"
              v-model="scope.row.boolStatusEnum"
              :disabled="!checkPermission(permission.edit)"
              active-color="#13ce66"
              :active-value="processingEnum.PROCESS.V"
              :inactive-value="processingEnum.PAUSE.V"
              @change="changeStatus(scope.row, scope.row.boolStatusEnum)"
            />
          </template>
        </el-table-column>
        <el-table-column
          v-if="checkPermission([...permission.edit, ...permission.del])"
          label="操作"
          width="180px"
          align="center"
          fixed="right"
        >
          <template v-slot="scope">
            <template v-if="scope.row.isModify">
              <common-button type="info" size="mini" @click="rowCancel(scope.row)">取消</common-button>
              <common-button type="primary" size="mini" @click="rowSubmit(scope.row)">保存</common-button>
            </template>
            <template v-else>
              <common-button size="small" class="el-icon-edit" type="primary" @click="editRow(scope.row)" />
              <el-popconfirm
                confirm-button-text="确定"
                cancel-button-text="取消"
                icon-color="red"
                title="确定删除吗?"
                @confirm="deleteRow(scope.row)"
              >
                <template #reference>
                  <common-button size="small" class="el-icon-delete" type="danger"/>
                </template>
              </el-popconfirm>
            </template>
          </template>
        </el-table-column>
      </common-table>
      <!--分页组件-->
      <pagination />
      <mForm/>
    </template>
  </div>
</template>

<script setup>
import crudApi, { editStatus } from '@/api/plan/technical-manage/enclosure'
import { getContractTechInfo } from '@/api/contract/project'
import { ref, watch, provide } from 'vue'
import checkPermission from '@/utils/system/check-permission'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import { mapGetters } from '@/store/lib'
import mHeader from './module/header'
import mForm from './module/form'
import { DP } from '@/settings/config'
import { processingEnum } from '@enum-ms/plan'
import { ElMessage, ElMessageBox } from 'element-plus'
import { isNotBlank } from '@data-type/index'
import { TechnologyTypeAllEnum } from '@enum-ms/contract'
import { validate } from '@compos/form/use-table-validate'
import { enclosureListPM as permission } from '@/page-permission/plan'

const { globalProject, globalProjectId } = mapGetters(['globalProject', 'globalProjectId'])

const plateOption = ref([])
const totalTechInfo = ref({})
const typeProp = { key: 'id', label: 'plateType', value: 'id' }
const trussProp = { key: 'id', label: 'serialNumber', value: 'id' }
provide('plateOption', plateOption)

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const { crud, columns, CRUD } = useCRUD(
  {
    title: '围护清单',
    sort: ['id.desc'],
    permission: { ...permission },
    optShow: { ...optShow },
    requiredQuery: ['areaId'],
    crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)
const { maxHeight } = useMaxHeight({
  wrapperBox: '.enclosureList',
  paginate: true,
  extraHeight: 40
})

const bendingRules = {
  serialNumber: [{ required: true, message: '请输入编号', trigger: 'blur' }],
  unfoldedWidth: [{ required: true, message: '展开宽度必填', trigger: 'change' }],
  thickness: [{ required: true, message: '厚度必填', trigger: 'change' }],
  length: [{ required: true, message: '单长必填', trigger: 'change' }],
  quantity: [{ required: true, message: '数量必填', trigger: 'change' }],
  totalArea: [{ required: true, message: '总面积必填', trigger: 'change' }],
  totalLength: [{ required: true, message: '总长度必填', trigger: 'change' }],
  color: [{ required: true, message: '请输入颜色', trigger: 'blur' }]
}

const otherRules = {
  serialNumber: [{ required: true, message: '请输入编号', trigger: 'blur' }],
  plateId: [{ required: true, message: '请选择版型', trigger: 'change' }],
  width: [{ required: true, message: '有效宽度必填', trigger: 'change' }],
  thickness: [{ required: true, message: '厚度必填', trigger: 'change' }],
  length: [{ required: true, message: '单长必填', trigger: 'change' }],
  quantity: [{ required: true, message: '数量必填', trigger: 'change' }],
  totalArea: [{ required: true, message: '总面积必填', trigger: 'change' }],
  totalLength: [{ required: true, message: '总长度必填', trigger: 'change' }]
}

const colorRules = {
  serialNumber: [{ required: true, message: '请输入编号', trigger: 'blur' }],
  plateId: [{ required: true, message: '请选择版型', trigger: 'change' }],
  width: [{ required: true, message: '有效宽度必填', trigger: 'change' }],
  thickness: [{ required: true, message: '厚度必填', trigger: 'change' }],
  length: [{ required: true, message: '单长必填', trigger: 'change' }],
  quantity: [{ required: true, message: '数量必填', trigger: 'change' }],
  totalArea: [{ required: true, message: '总面积必填', trigger: 'change' }],
  totalLength: [{ required: true, message: '总长度必填', trigger: 'change' }],
  color: [{ required: true, message: '请输入颜色', trigger: 'blur' }]
}

function wrongCellMask({ row, column }) {
  if (!row) return
  let rules = {}
  if (crud.query.category === TechnologyTypeAllEnum.BENDING.V) {
    rules = bendingRules
  } else if (crud.query.category === TechnologyTypeAllEnum.PROFILED_PLATE.V) {
    rules = colorRules
  } else {
    rules = otherRules
  }
  let flag = true
  if (row.verify && Object.keys(row.verify) && Object.keys(row.verify).length > 0) {
    if (row.verify[column.property] === false) {
      flag = validate(column.property, rules[column.property], row[column.property], row)
    }
    if (flag) {
      row.verify[column.property] = true
    }
  }
  return flag ? '' : 'mask-td'
}

watch(
  () => globalProjectId.value,
  (val) => {
    if (val) {
      getTechInfo()
      crud.query.projectId = globalProjectId
      crud.toQuery()
    }
  },
  { immediate: true, deep: true }
)

async function changeStatus(data, val) {
  try {
    const messageName = val === 1 ? '启用' : '暂停'
    await ElMessageBox.confirm('确定' + messageName + '?', '提示', {
      confirmButtonText: '确定',
      cancelButtonText: '取消',
      type: 'warning'
    })
    await editStatus(data.id)
    crud.notify(`“${data.name}”已【${messageName}】`, CRUD.NOTIFICATION_TYPE.SUCCESS)
    crud.toQuery()
  } catch (error) {
    console.log('修改围护状态', error)
  }
}
function getPlate() {
  plateOption.value = crud.query.category !== TechnologyTypeAllEnum.BENDING.V ? totalTechInfo.value[crud.query.category] : []
}
async function getTechInfo() {
  try {
    const data = await getContractTechInfo(globalProjectId.value)
    if (isNotBlank(data)) {
      totalTechInfo.value = {
        [TechnologyTypeAllEnum.PROFILED_PLATE.V]: data.profiledPlateList || [],
        [TechnologyTypeAllEnum.PRESSURE_BEARING_PLATE.V]: data.pressureBearingPlateList || [],
        [TechnologyTypeAllEnum.TRUSS_FLOOR_PLATE.V]: data.trussFloorPlateList || [],
        [TechnologyTypeAllEnum.SANDWICH_BOARD.V]: data.sandwichBoardList || []
      }
    }
    getPlate()
  } catch (error) {
    console.log('获取技术交底', error)
  }
}
function plateChange(row, index) {
  const choseVal = plateOption.value.find(v => v.id === row.plateId)
  if (crud.query.category === TechnologyTypeAllEnum.TRUSS_FLOOR_PLATE.V) {
    crud.data[index].plate = choseVal.serialNumber
  } else {
    crud.data[index].plate = choseVal.plateType
    crud.data[index].brand = choseVal.brand
    crud.data[index].thickness = choseVal.thickness
    crud.data[index].color = choseVal.colour
  }
  crud.data[index].width = choseVal.effectiveWidth
  getTotalData(row)
}

function getTotalData(row) {
  if (row.length && row.quantity) {
    row.totalLength = row.length * row.quantity / 1000
  }
  if (crud.query.category === TechnologyTypeAllEnum.BENDING.V) {
    if (row.length && row.quantity && row.unfoldedWidth) {
      row.totalArea = row.unfoldedWidth * row.length * row.quantity / 1000000
    }
  } else {
    if (row.length && row.quantity && row.width) {
      row.totalArea = row.width * row.length * row.quantity / 1000000
    }
  }
}

function tableAdd() {
  if (crud.query.category !== TechnologyTypeAllEnum.BENDING.V && plateOption.value.length <= 0) {
    ElMessage.error('请先前往合同管理添加技术交底')
    return
  }
  crud.data.unshift({
    id: undefined,
    areaId: crud.query.areaId,
    isModify: true,
    dataIndex: crud.data.length
  })
}
const originRow = ref({})
function editRow(row) {
  originRow.value = JSON.parse(JSON.stringify(row))
  const chosePlate = plateOption.value.find(k => k.plateType === row.plate)
  row.plateId = chosePlate.id
  row.isModify = true
}
async function deleteRow(row) {
  try {
    await crudApi.del(row.id)
    crud.notify(`删除成功`, CRUD.NOTIFICATION_TYPE.SUCCESS)
    crud.toQuery()
  } catch (e) {
    console.log('删除', e)
  }
}
function rowCancel(row) {
  if (!row.id) {
    const index = crud.data.findIndex(v => v.dataIndex === row.dataIndex)
    crud.data.splice(index, 1)
  } else {
    row = Object.assign(row, JSON.parse(JSON.stringify(originRow.value)))
    row.isModify = false
  }
}

async function rowSubmit(row) {
  if (crud.query.category !== TechnologyTypeAllEnum.BENDING.V) {
    crud.data.map(v => {
      if (!v.isModify && v.plate) {
        const chosePlate = plateOption.value.find(k => k.plateType === v.plate)
        v.plateId = chosePlate.id
      }
    })
  }
  let rules = {}
  if (crud.query.category === TechnologyTypeAllEnum.BENDING.V) {
    rules = bendingRules
  } else if (crud.query.category === TechnologyTypeAllEnum.PROFILED_PLATE.V) {
    rules = colorRules
  } else {
    rules = otherRules
  }
  let flag = true
  row.verify = {}
  for (const rule in rules) {
    row.verify[rule] = validate(rule, rules[rule], row[rule], row)
    if (!row.verify[rule]) {
      flag = false
    }
  }
  if (!flag) {
    ElMessage.error('请填写表格中标红数据')
    return
  }
  const messageName = row.id ? '修改' : '新增'
  try {
    if (row.id) {
      await crudApi.edit(row)
    } else {
      await crudApi.add(row)
    }
    crud.notify(`${messageName}成功`, CRUD.NOTIFICATION_TYPE.SUCCESS)
    row.isModify = false
  } catch (e) {
    console.log(messageName, e)
  }
}

CRUD.HOOK.handleRefresh = (crud, data) => {
  if (data.data.content.length > 0) {
    data.data.content.forEach(v => {
      v.type = crud.query.type
    })
  }
  return data
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
.enclosure-table{
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
