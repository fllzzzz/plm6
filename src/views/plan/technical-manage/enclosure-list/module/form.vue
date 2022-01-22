<template>
  <common-drawer
    append-to-body
    :close-on-click-modal="false"
    :before-close="crud.cancelCU"
    :visible="crud.status.cu > 0"
    :title="crud.query.category?crud.status.title+'('+TechnologyTypeAllEnum.VL[crud.query.category]+')':crud.status.title"
    :wrapper-closable="false"
    size="90%"
  >
    <template #titleRight>
      <common-button :loading="crud.status.cu === 2" type="primary" size="mini" @click="crud.submitCU">确认</common-button>
    </template>
    <template #content>
      <el-form ref="formRef" :model="form" size="small" label-width="140px">
        <common-table
          ref="detailRef"
          border
          :data="form.list"
          :max-height="300"
          style="width: 100%"
          class="table-form"
          :cell-class-name="wrongCellMask"
        >
          <el-table-column label="序号" type="index" align="center" width="50" />
          <el-table-column
            key="name"
            prop="name"
            :show-overflow-tooltip="true"
            label="名称"
            min-width="100"
          >
            <template v-slot="scope">
              <el-input
                v-model="scope.row.name"
                placeholder="名称"
                maxlength="20"
                style="width:100%;"
              />
            </template>
          </el-table-column>
          <el-table-column
            key="serialNumber"
            prop="serialNumber"
            :show-overflow-tooltip="true"
            label="编号"
            min-width="90px"
          >
            <template v-slot="scope">
              <el-input
                v-model="scope.row.serialNumber"
                placeholder="编号"
                maxlength="10"
                style="width:100%;"
              />
            </template>
          </el-table-column>
          <el-table-column
            v-if="crud.query.category!==TechnologyTypeAllEnum.BENDING.V"
            key="plateId"
            prop="plateId"
            :show-overflow-tooltip="true"
            label="版型"
            min-width="100px"
          >
            <template v-slot="scope">
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
          </el-table-column>
          <el-table-column
            v-if="crud.query.category!==TechnologyTypeAllEnum.BENDING.V"
            key="width"
            prop="width"
            :show-overflow-tooltip="true"
            :label="crud.query.category===TechnologyTypeAllEnum.SANDWICH_BOARD.V?'宽度\n(mm)':`有效宽度\n(mm)`"
            min-width="100px"
          >
            <template v-slot="scope">
              <el-input-number
                v-if="crud.query.category!==TechnologyTypeAllEnum.PROFILED_PLATE.V && crud.query.category!==TechnologyTypeAllEnum.PRESSURE_BEARING_PLATE.V"
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
            v-if="crud.query.category===TechnologyTypeAllEnum.BENDING.V"
            key="unfoldedWidth"
            prop="unfoldedWidth"
            :show-overflow-tooltip="true"
            :label="`展开宽度\n(mm)`"
            min-width="100px"
          >
            <template v-slot="scope">
              <el-input-number
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
            </template>
          </el-table-column>
          <el-table-column
            v-if="crud.query.category!==TechnologyTypeAllEnum.TRUSS_FLOOR_PLATE.V"
            key="thickness"
            prop="thickness"
            :show-overflow-tooltip="true"
            :label="`板厚\n(mm)`"
            align="left"
            min-width="100px"
          >
            <template v-slot="scope">
              <el-input-number
                v-model.number="scope.row.thickness"
                :min="0"
                :max="99999999999"
                :step="1"
                :precision="DP.MES_ENCLOSURE_T__MM"
                placeholder="板厚"
                controls-position="right"
                style="width:100%"
              />
            </template>
          </el-table-column>
          <el-table-column
            key="length"
            prop="length"
            :show-overflow-tooltip="true"
            :label="`单长\n(㎜)`"
            align="left"
            min-width="100px"
          >
            <template v-slot="scope">
              <el-input-number
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
            </template>
          </el-table-column>
          <el-table-column
            key="quantity"
            prop="quantity"
            :label="'数量(张)'"
            align="left"
            min-width="100px"
          >
            <template v-slot="scope">
              <el-input-number
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
            </template>
          </el-table-column>
          <el-table-column
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
            v-if="crud.query.category!==TechnologyTypeAllEnum.SANDWICH_BOARD.V && crud.query.category!==TechnologyTypeAllEnum.TRUSS_FLOOR_PLATE.V"
            key="brand"
            prop="brand"
            :show-overflow-tooltip="true"
            label="品牌"
            width="100px"
          >
            <template v-slot="scope">
              <el-input
                v-model="scope.row.brand"
                placeholder="品牌"
                maxlength="10"
                style="width:100%;"
              />
            </template>
          </el-table-column>
          <el-table-column
            v-if="crud.query.category!=TechnologyTypeAllEnum.TRUSS_FLOOR_PLATE.V && crud.query.category!=TechnologyTypeAllEnum.PRESSURE_BEARING_PLATE.V"
            key="color"
            prop="color"
            :show-overflow-tooltip="true"
            label="颜色"
            width="100px"
          >
            <template v-slot="scope">
              <el-input
                v-model="scope.row.color"
                placeholder="颜色"
                maxlength="10"
                style="width:100%;"
              />
            </template>
          </el-table-column>
          <el-table-column label="操作" align="center">
            <template v-slot="scope">
              <common-button size="small" class="el-icon-delete" type="danger" @click="deleteRow(scope.$index)" />
            </template>
          </el-table-column>
        </common-table>
        <div class="add-row-box">
          <common-button
            size="mini"
            icon="el-icon-circle-plus-outline"
            type="warning"
            style="margin-right: 15px"
            @click="addRow()"
            >继续添加</common-button>
        </div>
      </el-form>
    </template>
  </common-drawer>
</template>

<script setup>
import { ref, inject } from 'vue'
import { regForm } from '@compos/use-crud'
import { ElMessage } from 'element-plus'
import { TechnologyTypeAllEnum } from '@enum-ms/contract'
import { DP } from '@/settings/config'
import { validate } from '@compos/form/use-table-validate'

const formRef = ref()
const detailRef = ref()
const defaultForm = {
  list: []
}
const typeProp = { key: 'id', label: 'plateType', value: 'id' }
const trussProp = { key: 'id', label: 'serialNumber', value: 'id' }
const plateOption = inject('plateOption')

const { CRUD, crud, form } = regForm(defaultForm, formRef)

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

const trussRules = {
  serialNumber: [{ required: true, message: '请输入编号', trigger: 'blur' }],
  plateId: [{ required: true, message: '请选择版型', trigger: 'change' }],
  width: [{ required: true, message: '有效宽度必填', trigger: 'change' }],
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
  } else if (crud.query.category === TechnologyTypeAllEnum.SANDWICH_BOARD.V || crud.query.category === TechnologyTypeAllEnum.PROFILED_PLATE.V) {
    rules = colorRules
  } else if (crud.query.category === TechnologyTypeAllEnum.TRUSS_FLOOR_PLATE.V) {
    rules = trussRules
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

function deleteRow(index) {
  form.list.splice(index, 1)
}

function addRow() {
  form.list.push({
    areaId: crud.query.areaId
  })
}

function plateChange(row, index) {
  const choseVal = plateOption.value.find(v => v.id === row.plateId)
  console.log(choseVal)
  if (crud.query.category === TechnologyTypeAllEnum.TRUSS_FLOOR_PLATE.V) {
    form.list[index].plate = choseVal.serialNumber
  } else {
    form.list[index].plate = choseVal.plateType
    form.list[index].brand = choseVal.brand
    form.list[index].thickness = choseVal.thickness
    form.list[index].color = choseVal.colour
  }
  form.list[index].width = choseVal.effectiveWidth
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

CRUD.HOOK.beforeValidateCU = (crud, form) => {
  if (crud.form.list.length <= 0) {
    ElMessage({ message: '请先添加围护信息', type: 'error' })
    return false
  }
  let rules = {}
  if (crud.query.category === TechnologyTypeAllEnum.BENDING.V) {
    rules = bendingRules
  } else if (crud.query.category === TechnologyTypeAllEnum.SANDWICH_BOARD.V || crud.query.category === TechnologyTypeAllEnum.PROFILED_PLATE.V) {
    rules = colorRules
  } else if (crud.query.category === TechnologyTypeAllEnum.TRUSS_FLOOR_PLATE.V) {
    rules = trussRules
  } else {
    rules = otherRules
  }
  let flag = true
  crud.form.list.map(row => {
    row.verify = {}
    for (const rule in rules) {
      row.verify[rule] = validate(rule, rules[rule], row[rule], row)
      if (!row.verify[rule]) {
        flag = false
      }
    }
  })
  if (!flag) {
    ElMessage.error('请填写表格中标红数据')
    return false
  }
  crud.form.areaId = crud.query.areaId
}
</script>
<style lang="scss" scoped>
::v-deep(.el-input-number .el-input__inner) {
  text-align: left;
}
.table-form {
  ::v-deep(.el-input__inner) {
    padding: 0;
    padding-left: 5px;
  }
}
.add-row-box {
  text-align: center;
  margin-top: 20px;
}
</style>
