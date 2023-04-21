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
          :max-height="maxHeight"
          return-source-data
          :showEmptySymbol="false"
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
            :key="technicalTypeStatus ? 'plateId' : 'plate'"
            :prop="technicalTypeStatus ? 'plateId' : 'plate'"
            :show-overflow-tooltip="true"
            label="版型"
            min-width="100px"
          >
            <template v-slot="scope">
              <common-select
                v-if="technicalTypeStatus"
                v-model="scope.row.plateId"
                :options="plateOption"
                :type="'other'"
                :dataStructure="crud.query.category===TechnologyTypeAllEnum.TRUSS_FLOOR_PLATE.V?trussProp:typeProp"
                size="small"
                placeholder="版型"
                @change="plateChange(scope.row,scope.$index)"
              />
              <common-select
                v-else
                v-model="scope.row.plate"
                :options="enclosureDictKV?.['plate_type']"
                :dataStructure="defaultProp"
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
            v-if="crud.query.category===TechnologyTypeAllEnum.BENDING.V || (crud.query.category===TechnologyTypeAllEnum.PROFILED_PLATE.V || crud.query.category===TechnologyTypeAllEnum.PRESSURE_BEARING_PLATE.V)"
            key="unfoldedWidth"
            prop="unfoldedWidth"
            :show-overflow-tooltip="true"
            :label="`展开宽度\n(mm)`"
            min-width="100px"
          >
            <template v-slot="scope">
              <el-input-number
                v-if="crud.query.category===TechnologyTypeAllEnum.BENDING.V"
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
            v-if="crud.query.category===TechnologyTypeAllEnum.BENDING.V"
            key="bendTimes"
            prop="bendTimes"
            :show-overflow-tooltip="true"
            :label="`折弯次数`"
            min-width="100px"
          >
            <template v-slot="scope">
              <el-input-number
                v-model.number="scope.row.bendTimes"
                :min="0"
                :max="99999999999"
                :step="1"
                :precision="0"
                placeholder="折弯次数"
                controls-position="right"
                style="width:100%"
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
                @change="thicknessChange(scope.row)"
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
            v-if="crud.query.category!==TechnologyTypeAllEnum.SANDWICH_BOARD.V"
            key="weight"
            prop="weight"
            :label="`总重量(kg)`"
            align="left"
            min-width="100px"
          >
            <template v-slot="scope">
              {{ scope.row.weight ? scope.row.weight.toFixed(DP.COM_WT__KG) : '-' }}
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
            v-if="crud.query.category!=TechnologyTypeAllEnum.TRUSS_FLOOR_PLATE.V && crud.query.category!=TechnologyTypeAllEnum.PRESSURE_BEARING_PLATE.V&& crud.query.category!=TechnologyTypeAllEnum.PRESSURE_BEARING_PLATE.V && crud.query.category!=TechnologyTypeAllEnum.SANDWICH_BOARD.V"
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
import { getEnclosureDictList } from '@/api/contract/project'
import { ref, inject, watch, computed } from 'vue'

import { DP } from '@/settings/config'
import { TechnologyTypeAllEnum } from '@enum-ms/contract'

import { ElMessage } from 'element-plus'
import { regForm } from '@compos/use-crud'
import { validate } from '@compos/form/use-table-validate'
import useMaxHeight from '@compos/use-max-height'

const formRef = ref()
const detailRef = ref()
const enclosureDictKV = ref({}) // 围护配置
const defaultForm = {
  list: []
}
const typeProp = { key: 'id', label: 'plateType', value: 'id' }
const trussProp = { key: 'id', label: 'serialNumber', value: 'id' }
const defaultProp = { key: 'name', label: 'name', value: 'name' }

const plateOption = inject('plateOption')
const technicalTypeStatus = inject('technicalTypeStatus') // 技术交底状态

const { CRUD, crud, form } = regForm(defaultForm, formRef)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.enclosureForm',
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
  width: [{ validator: validateWidth, message: '有效宽度必填', trigger: 'change' }],
  thickness: [{ validator: validateThickness, message: '厚度必填', trigger: 'change' }],
  length: [{ required: true, message: '单长必填', trigger: 'change' }],
  quantity: [{ required: true, message: '数量必填', trigger: 'change' }],
  totalArea: [{ required: true, message: '总面积必填', trigger: 'change' }],
  totalLength: [{ required: true, message: '总长度必填', trigger: 'change' }],
  bendTimes: [{ validator: validateBendTimes, message: '折弯次数必填', trigger: 'change' }],
  color: [{ validator: validateColor, message: '请输入颜色', trigger: 'blur' }]
}

// 禁用技术交底配置时，前端不加任何限制，后端处理
const tableRules = computed(() => {
  let data = {}
  if (technicalTypeStatus.value) {
    data = {
      plateId: [{ validator: validatePlateId, message: '请选择版型', trigger: 'change' }]
    }
  } else {
    data = {
      plate: [{ required: true, message: '请选择版型', trigger: 'change' }]
    }
  }
  return { ...rules, ...data }
})

watch(
  () => crud.query.category,
  () => {
    getConfig()
  },
  { immediate: true }
)

watch(
  () => technicalTypeStatus.value,
  () => {
    getConfig()
  },
  { immediate: true }
)

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

function deleteRow(index) {
  form.list.splice(index, 1)
}

function addRow() {
  form.list.push({
    areaId: crud.query.areaId
  })
}

function plateChange(row, index) {
  // 关联技术交底
  if (row.plateId) {
    const choseVal = plateOption.value.find(v => v.id === row.plateId)
    if (crud.query.category === TechnologyTypeAllEnum.TRUSS_FLOOR_PLATE.V) {
      form.list[index].plate = choseVal.serialNumber
      form.list[index].weightMeter = choseVal.weightMeter
    } else {
      form.list[index].plate = choseVal.plateType
      form.list[index].brand = choseVal.brand
      form.list[index].thickness = choseVal.thickness
      form.list[index].color = choseVal.colour
      form.list[index].unfoldedWidth = choseVal.unfoldedWidth
    }
    form.list[index].width = choseVal.effectiveWidth
    getTotalData(row)
  } else {
    const data = enclosureDictKV.value.KV?.[row.plate] || {}
    if (crud.query.category === TechnologyTypeAllEnum.TRUSS_FLOOR_PLATE.V) {
      form.list[index].width = +data.effectiveWidth
    } else if (crud.query.category === TechnologyTypeAllEnum.PROFILED_PLATE.V || crud.query.category === TechnologyTypeAllEnum.PRESSURE_BEARING_PLATE.V) {
      form.list[index].width = +data.effectiveWidth
      form.list[index].unfoldedWidth = +data.unfoldedWidth
    }
  }
}

function getTotalData(row) {
  if (row.length && row.quantity) {
    row.totalLength = row.length * row.quantity / 1000
    thicknessChange(row)
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

function thicknessChange(row) {
  if (crud.query.category === TechnologyTypeAllEnum.BENDING.V || (crud.query.category === TechnologyTypeAllEnum.PROFILED_PLATE.V || crud.query.category === TechnologyTypeAllEnum.PRESSURE_BEARING_PLATE.V)) {
    if (row.unfoldedWidth && row.thickness && row.totalLength) {
      row.weight = (row.unfoldedWidth / 1000) * row.thickness * row.totalLength * 7.85
    }
  } else if (crud.query.category === TechnologyTypeAllEnum.TRUSS_FLOOR_PLATE.V) {
    if (row.weightMeter && row.totalLength) {
      row.weight = row.weightMeter * row.totalLength
    }
  }
}

CRUD.HOOK.beforeValidateCU = (crud, form) => {
  if (crud.form.list.length <= 0) {
    ElMessage({ message: '请先添加围护信息', type: 'error' })
    return false
  }
  const rules = tableRules.value
  let flag = true
  crud.form.list.map(row => {
    row.verify = {}
    for (const rule in rules) {
      row.verify[rule] = validate(rule, rules[rule], row)
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
