<template>
  <div class="app-container">
    <template v-if="pageShow">
      <!--工具栏-->
      <div class="head-container">
        <mHeader
          :project-id="globalProjectId"
          @tableAdd="tableAdd"
          :table-data="totalTechInfo"
          :globalProject="globalProject"
          :typeOption="typeOption"
          @categoryChange="getPlate"
          :sumData="sumData"
          @sumChange="getData"
        />
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
        @sort-change="crud.handleSortChange"
        class="enclosure-table"
        :cell-class-name="wrongCellMask"
      >
        <el-table-column label="序号" type="index" align="center" width="50" fixed="left" />
         <el-table-column v-if="columns.visible('name')" key="name" prop="name" :show-overflow-tooltip="true" label="名称" min-width="120">
          <template v-slot="scope">
            <el-input v-if="scope.row.isModify" v-model="scope.row.name" placeholder="名称" maxlength="20" style="width: 100%" />
            <div v-else>{{ scope.row.name ? scope.row.name : '-' }}</div>
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('serialNumber') && crud.query.category !== TechnologyTypeAllEnum.BENDING.V"
          key="serialNumber"
          prop="serialNumber"
          :show-overflow-tooltip="true"
          label="编号"
          min-width="120px"
        >
          <template v-slot="scope">
            <el-input
              v-if="scope.row.isModify"
              v-model="scope.row.serialNumber"
              placeholder="编号"
              maxlength="10"
              style="width:100%;"
            />
            <div v-else>{{ scope.row.serialNumber }}</div>
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('serialNumber') && crud.query.category === TechnologyTypeAllEnum.BENDING.V"
          key="serialNumber"
          prop="serialNumber"
          :show-overflow-tooltip="true"
          label="编号"
          min-width="120px"
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
             <el-input
              v-if="scope.row.isModify"
              v-model="scope.row.serialNumber"
              placeholder="编号"
              maxlength="10"
              style="width:100%;"
            />
             <span v-else style="cursor: pointer" @dblclick="dbClick(scope.row)">{{ scope.row.serialNumber }}</span>
            <!-- <div>{{ scope.row.serialNumber }}</div> -->
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('plateId') && crud.query.category !== TechnologyTypeAllEnum.BENDING.V"
          :key="technicalTypeStatus ? 'plateId' : 'plate'"
          :prop="technicalTypeStatus ? 'plateId' : 'plate'"
          :show-overflow-tooltip="true"
          label="板型"
          min-width="100px"
        >
          <template v-slot="scope">
            <template v-if="scope.row.isModify && !scope.row.inProductionQuantity">
              <common-select
                v-if="technicalTypeStatus"
                v-model="scope.row.plateId"
                :options="plateOption"
                :type="'other'"
                :dataStructure="crud.query.category===TechnologyTypeAllEnum.TRUSS_FLOOR_PLATE.V?trussProp:typeProp"
                size="small"
                placeholder="板型"
                @change="plateChange(scope.row,scope.$index)"
              />
              <common-select
                v-else
                v-model="scope.row.plate"
                :options="enclosureDictKV?.['plate_type']"
                :dataStructure="{ key: 'name', label: 'name', value: 'name' }"
                size="small"
                placeholder="板型"
                @change="plateChange(scope.row,scope.$index)"
              />
            </template>
            <div v-else>{{ scope.row.plate ? scope.row.plate : '-' }}</div>
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('width') && crud.query.category !== TechnologyTypeAllEnum.BENDING.V"
          key="width"
          prop="width"
          :show-overflow-tooltip="true"
          :label="crud.query.category === TechnologyTypeAllEnum.SANDWICH_BOARD.V ? '宽度\n(mm)' : `有效宽度\n(mm)`"
          min-width="100px"
        >
          <template v-slot="scope">
            <el-input-number
              v-if="
                scope.row.isModify &&
                crud.query.category !== TechnologyTypeAllEnum.PROFILED_PLATE.V &&
                crud.query.category !== TechnologyTypeAllEnum.PRESSURE_BEARING_PLATE.V &&
                !scope.row.inProductionQuantity
              "
              v-model.number="scope.row.width"
              :min="0"
              :max="9999999999"
              :step="1"
              :precision="DP.MES_ENCLOSURE_W__MM"
              placeholder="有效宽度"
              controls-position="right"
              style="width: 100%"
              @change="getTotalData(scope.row)"
            />
            <div v-else>{{ scope.row.width ? scope.row.width.toFixed(DP.MES_ENCLOSURE_W__MM) : '-' }}</div>
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('unfoldedWidth') && (crud.query.category===TechnologyTypeAllEnum.BENDING.V || (crud.query.category===TechnologyTypeAllEnum.PROFILED_PLATE.V || crud.query.category===TechnologyTypeAllEnum.PRESSURE_BEARING_PLATE.V))"
          key="unfoldedWidth"
          prop="unfoldedWidth"
          :show-overflow-tooltip="true"
          :label="`展开宽度\n(mm)`"
          min-width="100px"
        >
          <template v-slot="scope">
            <el-input-number
              v-if="scope.row.isModify && !scope.row.inProductionQuantity && crud.query.category===TechnologyTypeAllEnum.BENDING.V"
              v-model.number="scope.row.unfoldedWidth"
              :min="0"
              :max="9999999999"
              :step="1"
              :precision="DP.MES_ENCLOSURE_W__MM"
              placeholder="展开宽度"
              controls-position="right"
              style="width: 100%"
              @change="getTotalData(scope.row)"
            />
            <div v-else>{{ scope.row.unfoldedWidth ? scope.row.unfoldedWidth.toFixed(DP.MES_ENCLOSURE_W__MM) : '-' }}</div>
          </template>
        </el-table-column>
        <el-table-column
          v-if="crud.query.category===TechnologyTypeAllEnum.BENDING.V"
          key="bendTimes"
          prop="bendTimes"
          :show-overflow-tooltip="true"
          :label="`折弯次数`"
          width="80px"
        >
          <template v-slot="scope">
            <el-input-number
              v-if="scope.row.isModify && !scope.row.inProductionQuantity"
              v-model.number="scope.row.bendTimes"
              :min="0"
              :max="9999999999"
              :step="1"
              :precision="0"
              placeholder="折弯次数"
              controls-position="right"
              style="width:100%"
            />
            <span v-else>{{scope.row.bendTimes? scope.row.bendTimes : '-'}}</span>
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('thickness') && crud.query.category !== TechnologyTypeAllEnum.TRUSS_FLOOR_PLATE.V"
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
              :max="9999999999"
              :step="1"
              :precision="DP.MES_ENCLOSURE_T__MM"
              placeholder="板厚"
              controls-position="right"
              style="width: 100%"
              @change="thicknessChange(scope.row)"
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
              :max="9999999999"
              :step="1"
              :precision="DP.MES_ENCLOSURE_L__MM"
              placeholder="单长"
              controls-position="right"
              style="width: 100%"
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
              :max="9999999999"
              :step="1"
              :precision="DP.MES_ENCLOSURE_L__MM"
              placeholder="数量"
              controls-position="right"
              style="width: 100%"
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
            {{ scope.row.totalArea ? scope.row.totalArea.toFixed(DP.COM_ENCLOSURE_AREA__M2) : '-' }}
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
          v-if="crud.query.category!==TechnologyTypeAllEnum.SANDWICH_BOARD.V"
          key="totalWeight"
          prop="totalWeight"
          :label="`总重量(kg)`"
          align="left"
          min-width="100px"
        >
          <template v-slot="scope">
            {{ scope.row.totalWeight ? scope.row.totalWeight.toFixed(DP.COM_WT__KG) : '-' }}
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('coating')"
          key="coating"
          prop="coating"
          :show-overflow-tooltip="true"
          label="涂层"
          width="100px"
        >
          <template v-slot="scope">
            <el-input v-if="scope.row.isModify" v-model="scope.row.coating" placeholder="涂层" maxlength="10" style="width: 100%" />
            <div v-else>{{ scope.row.coating ? scope.row.coating : '-' }}</div>
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('plating')"
          key="plating"
          prop="plating"
          :show-overflow-tooltip="true"
          label="镀层"
          width="80px"
        >
          <template v-slot="scope">
            <el-input v-if="scope.row.isModify" v-model="scope.row.plating" placeholder="镀层" maxlength="10" style="width: 100%" />
            <div v-else>{{ scope.row.plating ? scope.row.plating : '-' }}</div>
          </template>
        </el-table-column>
        <el-table-column
          v-if="
            columns.visible('brand') &&
            crud.query.category !== TechnologyTypeAllEnum.SANDWICH_BOARD.V &&
            crud.query.category !== TechnologyTypeAllEnum.TRUSS_FLOOR_PLATE.V
          "
          key="brand"
          prop="brand"
          :show-overflow-tooltip="true"
          label="品牌"
          width="100px"
        >
          <template v-slot="scope">
            <el-input v-if="scope.row.isModify" v-model="scope.row.brand" placeholder="品牌" maxlength="10" style="width: 100%" />
            <div v-else>{{ scope.row.brand ? scope.row.brand : '-' }}</div>
          </template>
        </el-table-column>
        <el-table-column
          v-if="
            columns.visible('color') &&
            crud.query.category != TechnologyTypeAllEnum.TRUSS_FLOOR_PLATE.V &&
            crud.query.category != TechnologyTypeAllEnum.PRESSURE_BEARING_PLATE.V &&
            crud.query.category != TechnologyTypeAllEnum.SANDWICH_BOARD.V
          "
          key="color"
          prop="color"
          :show-overflow-tooltip="true"
          label="颜色"
          width="80px"
        >
          <template v-slot="scope">
            <el-input v-if="scope.row.isModify" v-model="scope.row.color" placeholder="颜色" maxlength="10" style="width: 100%" />
            <div v-else>{{ scope.row.color ? scope.row.color : '-' }}</div>
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('remark')"
          key="remark"
          prop="remark"
          :show-overflow-tooltip="true"
          label="备注"
          min-width="120px"
        >
          <template v-slot="scope">
            <el-input v-if="scope.row.isModify" v-model="scope.row.remark" placeholder="备注" type="textarea" maxlength="200" style="width: 100%" />
            <div v-else>{{ scope.row.remark ? scope.row.remark : '-' }}</div>
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('draw') && crud.query.category === TechnologyTypeAllEnum.BENDING.V"
          key="draw"
          prop="draw"
          :show-overflow-tooltip="true"
          label="画图"
          min-width="190px"
        >
          <template v-slot="scope">
              <upload-btn
              :upload-fun="uploadBendingSingle"
              :data="{ id: scope.row.id }"
              :fileClassify="undefined"
              :accept="'.jpg,.jpeg,.png'"
              success-msg="上传成功"
              :btn-name="`上传`"
              btn-type="warning"
              btn-size="mini"
              @success="uploadSuccess"
              style="display:inline-block;margin-right:5px;"
            />
            <common-button size="mini" type="primary" @click="handleDraw(scope.row)" :disabled="scope.row.isModify" v-permission="permission.draw">{{
              scope.row.attachmentId ? '换图' : '画图'
            }}</common-button>
            <common-button size="mini" type="primary" icon="el-icon-view" v-permission="permission.drawDownload" @click="drawingPreview(scope.row)" v-if="scope.row.attachmentId"/>
          </template>
        </el-table-column>
        <el-table-column v-if="columns.visible('createTime')" key="createTime" prop="createTime" label="上传时间" align="center" show-overflow-tooltip width="110">
          <template v-slot="scope">
            {{scope.row.createTime?parseTime(scope.row.createTime,'{y}-{m}-{d}'):'-'}}
          </template>
        </el-table-column>
        <el-table-column v-if="columns.visible('userName')" key="userName" prop="userName" label="上传人" align="center" show-overflow-tooltip width="100" />
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
              <common-button size="small" class="el-icon-edit" type="primary" @click="editRow(scope.row)" v-permission="permission.edit"/>
              <el-popconfirm
                confirm-button-text="确定"
                cancel-button-text="取消"
                icon-color="red"
                title="确定删除吗?"
                @confirm="deleteRow(scope.row)"
                v-if="checkPermission(permission.del)"
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
      <mForm />
      <!-- 画图 -->
      <SimpleDrawing ref="simpleDrawRef" v-model="drawVisible" :current="currentRow" @toQuery="crud.toQuery" />
      <!-- img预览 -->
      <drawing-img
        v-model="showDrawing"
        :serial-number="drawingRow?.serialNumber"
        :attachmentId="drawingRow?.attachmentId"
      />
    </template>
    <template v-else>
      <span style="color:red;font-size:13px;">当前项目内容没有包含围护制品，请到合同管理中进行配置</span>
    </template>
  </div>
</template>

<script setup>
import crudApi, { editStatus, uploadBendingSingle, getTotalSum } from '@/api/plan/technical-manage/enclosure'
import { getContractTechInfo, getEnclosureDictList } from '@/api/contract/project'
import { getTechnicalType } from '@/api/config/mes/base'
import { ref, watch, provide, computed } from 'vue'
import { ElMessage, ElMessageBox } from 'element-plus'

import { enclosureListPM as permission } from '@/page-permission/plan'
import checkPermission from '@/utils/system/check-permission'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import { mapGetters } from '@/store/lib'
import { parseTime } from '@/utils/date'

import { DP } from '@/settings/config'
import { processingEnum } from '@enum-ms/plan'
import { isNotBlank } from '@data-type/index'
import { TechnologyTypeAllEnum } from '@enum-ms/contract'
import { validate } from '@compos/form/use-table-validate'

import SimpleDrawing from '../components/simple-drawing'
import useDrawing from '@compos/use-drawing'
import drawingImg from '@comp-base/drawing-img.vue'
import uploadBtn from '@comp/file-upload/SingleFileUploadBtn'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import mForm from './module/form'

const { globalProject, globalProjectId } = mapGetters(['globalProject', 'globalProjectId'])
const { showDrawing, drawingRow, drawingPreview } = useDrawing({})

const plateOption = ref([])
const technicalTypeStatus = ref(true) // 技术交底状态
const totalTechInfo = ref({})
const simpleDrawRef = ref()
const typeProp = { key: 'id', label: 'plateType', value: 'id' }
const trussProp = { key: 'id', label: 'serialNumber', value: 'id' }
const pageShow = ref(true)
const sumData = ref({})

provide('plateOption', plateOption)
provide('technicalTypeStatus', technicalTypeStatus)
const drawVisible = ref(false)
const currentRow = ref({})
const typeOption = ref([])
const enclosureDictKV = ref({}) // 围护配置
const techOptions = [
  {
    name: '压型彩板',
    no: TechnologyTypeAllEnum.PROFILED_PLATE.V,
    alias: 'ENCLOSURE'
  },
  {
    name: '压型楼承板',
    no: TechnologyTypeAllEnum.PRESSURE_BEARING_PLATE.V,
    alias: 'ENCLOSURE'
  },
  {
    name: '桁架楼承板',
    no: TechnologyTypeAllEnum.TRUSS_FLOOR_PLATE.V,
    alias: 'ENCLOSURE'
  },
  {
    name: '夹芯板',
    no: TechnologyTypeAllEnum.SANDWICH_BOARD.V,
    alias: 'ENCLOSURE'
  },
  {
    name: '折边件',
    no: TechnologyTypeAllEnum.BENDING.V,
    alias: 'ENCLOSURE'
  }
]

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const { crud, columns, CRUD } = useCRUD(
  {
    title: '折边件清单',
    sort: ['id.desc'],
    permission: { ...permission },
    optShow: { ...optShow },
    requiredQuery: ['enclosurePlanId'],
    invisibleColumns: ['createTime', 'userName'],
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

// 折边件展开宽度校验
const validateUnfoldedWidth = (value, row) => {
  if (crud.query.category === TechnologyTypeAllEnum.BENDING.V) {
    if (!value) return false
    return true
  }
  return true
}

// 型号
const validatePlateId = (value, row) => {
  if (crud.query.category !== TechnologyTypeAllEnum.BENDING.V) {
    if (!value) return false
    return true
  }
  return true
}

// 有效宽度
const validateWidth = (value, row) => {
  if (crud.query.category !== TechnologyTypeAllEnum.BENDING.V) {
    if (!value) return false
    return true
  }
  return true
}

// 厚度
const validateThickness = (value, row) => {
  if (crud.query.category !== TechnologyTypeAllEnum.TRUSS_FLOOR_PLATE.V) {
    if (!value) return false
    return true
  }
  return true
}

// 折边件折弯次数
const validateBendTimes = (value, row) => {
  if (crud.query.category === TechnologyTypeAllEnum.BENDING.V) {
    if (!value) return false
    return true
  }
  return true
}

// 颜色
const validateColor = (value, row) => {
  if (crud.query.category === TechnologyTypeAllEnum.BENDING.V || crud.query.category === TechnologyTypeAllEnum.PROFILED_PLATE.V) {
    if (!value) return false
    return true
  }
  return true
}

const rules = {
  serialNumber: [{ required: true, message: '请输入编号', trigger: 'blur' }],
  unfoldedWidth: [{ validator: validateUnfoldedWidth, message: '展开宽度必填', trigger: 'change' }],
  // plateId: [{ validator: validatePlateId, message: '请选择板型', trigger: 'change' }],
  width: [{ validator: validateWidth, message: '有效宽度必填', trigger: 'change' }],
  thickness: [{ validator: validateThickness, message: '厚度必填', trigger: 'change' }],
  length: [{ required: true, message: '单长必填', trigger: 'change' }],
  quantity: [{ required: true, message: '数量必填', trigger: 'change' }],
  totalArea: [{ required: true, message: '总面积必填', trigger: 'change' }],
  totalLength: [{ required: true, message: '总长度必填', trigger: 'change' }],
  bendTimes: [{ validator: validateBendTimes, message: '折弯次数必填', trigger: 'change' }],
  color: [{ validator: validateColor, message: '请输入颜色', trigger: 'blur' }]
}

const tableRules = computed(() => {
  let data = {}
  if (technicalTypeStatus.value) {
    data = {
      plateId: [{ validator: validatePlateId, message: '请选择板型', trigger: 'change' }]
    }
  } else {
    data = {
      plate: [{ validator: validatePlateId, message: '请选择板型', trigger: 'change' }]
    }
  }
  return { ...rules, ...data }
})

function wrongCellMask({ row, column }) {
  if (!row) return
  const rules = tableRules.value
  let flag = true
  if (row.verify && Object.keys(row.verify) && Object.keys(row.verify).length > 0) {
    if (row.verify[column.property] === false) {
      flag = validate(column.property, rules[column.property], row)
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
      sumData.value = {}
      getTechInfo()
      crud.query.projectId = globalProjectId.value
      crud.toQuery()
    }
  },
  { immediate: true, deep: true }
)

watch(
  () => globalProject.value,
  (val) => {
    typeOption.value = []
    if (isNotBlank(val)) {
      techOptions.forEach((v) => {
        if (val.projectContentList.findIndex((k) => Number(k.no) === v.no) > -1) {
          typeOption.value.push(v)
        }
      })
      pageShow.value = typeOption.value.length > 0
    }
  },
  { deep: true, immediate: true }
)

watch(
  () => crud.query,
  (val) => {
    getData()
  },
  { deep: true, immediate: true }
)

function dbClick(row) {
  if (!row.attachmentId) {
    return
  }
  drawingPreview(row)
}

getTechnicalTypeStatus()

// 获取技术交底配置状态
async function getTechnicalTypeStatus() {
  try {
    const { technicalType = true } = await getTechnicalType()
    technicalTypeStatus.value = technicalType
    getConfig()
  } catch (error) {
    console.log('获取技术交底配置状态', error)
    technicalTypeStatus.value = true
  }
}

// 获取围护配置信息列表
async function getConfig() {
  enclosureDictKV.value = {}
  if (technicalTypeStatus.value || !crud.query.category || crud.query.category === TechnologyTypeAllEnum.BENDING.V) return
  try {
    const { content = [] } = await getEnclosureDictList(crud.query.category) || {}
    const plateKV = {}
    if (crud.query.category === TechnologyTypeAllEnum.TRUSS_FLOOR_PLATE.V) {
      enclosureDictKV.value['plate_type'] = content.map(row => {
        let effectiveWidth = ''
        row?.list.forEach(v => {
          if (v.name === 'effectiveWidth') {
            effectiveWidth = v.value
          }
        })
        plateKV[row.code] = { effectiveWidth }
        return { name: row.code }
      })
    } else {
      content.forEach((row) => {
        if (row.name === 'plate_type') {
          (row.plateTypeList || []).forEach(v => {
            plateKV[v.plateType] = v
          })
          enclosureDictKV.value[row.name] = (row.labels || []).map(v => {
            return {
              name: v
            }
          })
        }
      })
    }
    enclosureDictKV.value.KV = plateKV
  } catch (error) {
    console.log('获取围护配置信息列表', error)
  }
}

async function changeStatus(data, val) {
  try {
    const messageName = val === 1 ? '启用' : '暂停'
    await ElMessageBox.confirm('确定' + messageName + '?', '提示', {
      confirmButtonText: '确定',
      cancelButtonText: '取消',
      type: 'warning'
    })
    await editStatus(data.id, { boolStatusEnum: !!val })
    crud.notify(`“${data.name}”已【${messageName}】`, CRUD.NOTIFICATION_TYPE.SUCCESS)
    crud.toQuery()
  } catch (error) {
    console.log('修改围护状态', error)
    data.boolStatusEnum = data.boolStatusEnum === processingEnum.PROCESS.V ? processingEnum.PAUSE.V : processingEnum.PROCESS.V
  }
}
function uploadSuccess() {
  ElMessage.success('上传成功')
  crud.toQuery()
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
  if (row.plateId) {
    const choseVal = plateOption.value.find(v => v.id === row.plateId)
    if (crud.query.category === TechnologyTypeAllEnum.TRUSS_FLOOR_PLATE.V) {
      crud.data[index].plate = choseVal.serialNumber
      crud.data[index].weightMeter = choseVal.weightMeter
    } else {
      crud.data[index].plate = choseVal.plateType
      crud.data[index].brand = choseVal.brand
      crud.data[index].thickness = choseVal.thickness
      crud.data[index].color = choseVal.colour
      crud.data[index].unfoldedWidth = choseVal.unfoldedWidth
    }
    crud.data[index].width = choseVal.effectiveWidth
  } else {
    const data = enclosureDictKV.value.KV?.[row.plate] || {}
    if (crud.query.category === TechnologyTypeAllEnum.TRUSS_FLOOR_PLATE.V) {
      crud.data[index].width = +data.effectiveWidth
    } else if (crud.query.category === TechnologyTypeAllEnum.PROFILED_PLATE.V || crud.query.category === TechnologyTypeAllEnum.PRESSURE_BEARING_PLATE.V) {
      crud.data[index].width = +data.effectiveWidth
      crud.data[index].unfoldedWidth = +data.unfoldedWidth
    }
  }
  getTotalData(row)
}

function getTotalData(row) {
  if (row.length && row.quantity) {
    row.totalLength = (row.length * row.quantity) / 1000
    thicknessChange(row)
  }
  if (crud.query.category === TechnologyTypeAllEnum.BENDING.V || (crud.query.category === TechnologyTypeAllEnum.PROFILED_PLATE.V || crud.query.category === TechnologyTypeAllEnum.PRESSURE_BEARING_PLATE.V)) {
    if (row.length && row.quantity && row.unfoldedWidth) {
      row.totalArea = (row.unfoldedWidth * row.length * row.quantity) / 1000000
    }
  } else {
    if (row.length && row.quantity && row.width) {
      row.totalArea = (row.width * row.length * row.quantity) / 1000000
    }
  }
}

function thicknessChange(row) {
  if (crud.query.category === TechnologyTypeAllEnum.BENDING.V || (crud.query.category === TechnologyTypeAllEnum.PROFILED_PLATE.V || crud.query.category === TechnologyTypeAllEnum.PRESSURE_BEARING_PLATE.V)) {
    if (row.unfoldedWidth && row.thickness && row.totalLength) {
      row.totalWeight = (row.unfoldedWidth / 1000) * row.thickness * row.totalLength * 7.85
    }
  } else if (crud.query.category === TechnologyTypeAllEnum.TRUSS_FLOOR_PLATE.V) {
    if (row.weightMeter && row.totalLength) {
      row.totalWeight = row.weightMeter * row.totalLength
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
  if (technicalTypeStatus.value) {
    const chosePlate = plateOption.value.find((k) => k.plateType === row.plate)
    row.plateId = isNotBlank(chosePlate) ? chosePlate.id : undefined
  }
  row.isModify = true
}
async function deleteRow(row) {
  try {
    await crudApi.del(row.id)
    crud.notify(`删除成功`, CRUD.NOTIFICATION_TYPE.SUCCESS)
    getData()
    crud.toQuery()
  } catch (e) {
    console.log('删除', e)
  }
}
function rowCancel(row) {
  if (!row.id) {
    const index = crud.data.findIndex((v) => v.dataIndex === row.dataIndex)
    crud.data.splice(index, 1)
  } else {
    row = Object.assign(row, JSON.parse(JSON.stringify(originRow.value)))
    row.isModify = false
  }
}

async function rowSubmit(row) {
  if (crud.query.category !== TechnologyTypeAllEnum.BENDING.V) {
    crud.data.map((v) => {
      if (!v.isModify && v.plate && technicalTypeStatus.value) {
        const chosePlate = plateOption.value.find((k) => k.plateType === v.plate)
        v.plateId = chosePlate.id
      }
    })
  }
  const rules = tableRules.value
  let flag = true
  row.verify = {}
  for (const rule in rules) {
    row.verify[rule] = validate(rule, rules[rule], row)
    if (!row.verify[rule]) {
      flag = false
    }
  }
  console.log(row)
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
    getData()
    row.isModify = false
  } catch (e) {
    console.log(messageName, e)
  }
}

function handleDraw(row) {
  drawVisible.value = true
  currentRow.value = row
}

CRUD.HOOK.handleRefresh = (crud, data) => {
  if (data.data.content.length > 0) {
    data.data.content.forEach((v) => {
      v.type = crud.query.type
    })
  }
  return data
}

async function getData() {
  if (crud.query.enclosurePlanId && crud.query.category) {
    try {
      sumData.value = await getTotalSum({ ...crud.query })
    } catch (e) {
      console.log('获取围护汇总', e)
    }
  } else {
    sumData.value = {}
  }
}

CRUD.HOOK.afterSubmit = (crud, data) => {
  getData()
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
.enclosure-table {
  ::v-deep(.el-select .el-input__inner) {
    padding-left: 2px;
    padding-right: 5px;
  }
  ::v-deep(.el-input-number .el-input__inner, .el-input__inner) {
    text-align: left;
    padding: 0 5px;
  }
  ::v-deep(.el-table .cell) {
    padding-left: 2px;
    padding-right: 2px;
  }
}
</style>
