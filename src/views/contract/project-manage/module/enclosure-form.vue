<template>
  <div v-loading="loading" class="app-container">
    <el-radio-group v-model="boardType" size="small" class="filter-item">
      <el-radio-button
        v-for="item in TechnologyTypeEnum.ENUM"
        :key="item.V"
        :label="item.V"
        :disabled="props.showItem.indexOf(item.V)<0"
      >
        {{ item.L }}{{ tableData[item.V] && tableData[item.V].length ? `(${tableData[item.V].length})`:'' }}
      </el-radio-button>
    </el-radio-group>
    <el-form v-show="boardType" ref="formRef" :model="form">
      <div v-for="(content,index) in FIELD_INFO[boardType]" :key="index" style="margin-top:10px;">
        <span class="form-title">{{ content.type }}</span>
        <div class="form-row">
          <el-form-item
            v-for="(item,index2) in content.fields"
            :key="item.field+index2"
            :prop="item.field"
            :rules="item.rules?item.rules:[]"
          >
            <template v-if="boardType!=TechnologyTypeEnum.STRUCTURE.V && boardType!=TechnologyTypeEnum.TRUSS_FLOOR_PLATE.V">
              <template v-if="item.field==='plateType' && boardType!==TechnologyTypeEnum.SANDWICH_BOARD.V">
                <el-select
                  v-model="form[item.field]"
                  clearable
                  size="small"
                  :placeholder="item.name+ `${item.unit?'，单位:'+item.unit:''}`"
                  style="min-width: 120px;margin-right:10px;"
                  class="input-underline"
                  @change="plateTypeChange"
                >
                  <el-option
                    v-for="(option,i) in typeDict[item.dict]"
                    :key="i"
                    :label="option"
                    :value="option"
                  />
                </el-select>
              </template>
              <el-select
                v-else
                v-model="form[item.field]"
                filterable
                clearable
                size="small"
                :placeholder="item.name+ `${item.unit?'，单位:'+item.unit:''}`"
                style="min-width: 120px;margin-right:10px;"
                class="input-underline"
                @blur="(e)=>{handleBlur(e,item.field,item.dict)}"
              >
                <el-option
                  v-for="(option,i) in typeDict[item.dict]"
                  :key="i"
                  :label="option"
                  :value="option"
                />
              </el-select>
            </template>
            <template v-else>
              <el-select
                v-if="boardType==TechnologyTypeEnum.TRUSS_FLOOR_PLATE.V"
                v-model="form[item.field]"
                filterable
                clearable
                size="small"
                :placeholder="item.name+ `${item.unit?'，单位:'+item.unit:''}`"
                style="min-width: 120px;margin-right:10px;"
                class="input-underline"
                @blur="(e)=>{handleBlur(e,item.field)}"
              >
                <el-option
                  v-for="(option,i) in trussDict"
                  :key="i"
                  :label="option.code"
                  :value="option.code"
                />
              </el-select>
              <el-select
                v-else
                v-model="form[item.field]"
                filterable
                clearable
                size="small"
                :placeholder="'产品种类'"
                style="min-width: 120px;margin-right:10px;"
                class="input-underline"
                @blur="(e)=>{handleBlur(e,item.field)}"
              >
                <el-option
                  v-for="option in showCategory"
                  :key="option.id"
                  :label="option.name"
                  :value="option.name"
                />
              </el-select>
            </template>
          </el-form-item>
        </div>
      </div>
      <div v-if="boardType==TechnologyTypeEnum.STRUCTURE.V">
        <div class="form-title" style="margin-bottom:10px;">技术要求描述</div>
        <el-form-item prop="techDesc">
          <el-input
            v-model="form.techDesc"
            type="textarea"
            maxlength="200"
            show-word-limit
            rows="2"
            placeholder="技术要求描述"
            style="width: 420px;"
          />
        </el-form-item>
      </div>
      <!-- 数量 -->
      <div style="margin-top:10px;" v-else>
        <span class="form-title">数量(m)</span>
        <el-form-item style="margin-top: 10px;" prop="quantity">
          <el-input-number
            v-model="form.quantity"
            placeholder="数量"
            size="small"
            class="input-underline"
            :precision="2"
            :controls="false"
            clearable
          />
        </el-form-item>
      </div>
      <div>
        <span style="float:right;margin-bottom:10px;">
          <common-button size="small" type="success" @click="addRow">保存继续添加</common-button>
          <common-button size="small" type="warning" @click="reset">重置</common-button>
        </span>
      </div>
    </el-form>
    <!-- 表格 -->
    <component
      :is="currentView"
      :table-data="tableData[boardType]"
      :is-show="false"
      @edit="editRow"
    />
  </div>
</template>

<script setup>
import { ref, defineProps, watch, computed, defineExpose, nextTick } from 'vue'
import { TechnologyTypeEnum } from '@enum-ms/contract'
import { getEnclosureDictList } from '@/api/contract/project'
import { isNotBlank } from '@data-type/index'
import { ElMessage, ElRadioGroup } from 'element-plus'
import sandwichTable from './enclosure-table/sandwich-table'
import pressedColorTable from './enclosure-table/pressed-color-table'
import pressedSupportTable from './enclosure-table/pressed-support-table'
import structureTable from './enclosure-table/structure-table'
import trussSupportTable from './enclosure-table/truss-support-table'

const props = defineProps({
  initForm: {
    type: Object,
    default: () => {}
  },
  showItem: {
    type: Array,
    default: () => {
      return []
    }
  },
  showCategory: {
    type: Array,
    default: () => {
      return []
    }
  },
  defaultType: {
    type: [String, Number],
    default: undefined
  }
})
const boardType = ref()
const tableData = ref({
  [TechnologyTypeEnum.STRUCTURE.V]: [],
  [TechnologyTypeEnum.PROFILED_PLATE.V]: [],
  [TechnologyTypeEnum.TRUSS_FLOOR_PLATE.V]: [],
  [TechnologyTypeEnum.PRESSURE_BEARING_PLATE.V]: [],
  [TechnologyTypeEnum.SANDWICH_BOARD.V]: []
})
const form = ref({
  dict: {}
})
const typeDict = ref({})
const trussDict = ref([])
const loading = ref(false)
const formRef = ref()
const isEditing = ref(false)
const plateTypeData = ref([])

watch(
  () => boardType.value,
  (newVal, oldVal) => {
    loading.value = false
    if (isEditing.value) {
      formRef.value.validate((valid) => {
        if (valid) {
          const row = Object.assign({}, form.value)
          tableData.value[oldVal].push(row)
        }
      })
    }
    if (boardType.value) {
      reset()
    }
    if (boardType.value && boardType.value !== TechnologyTypeEnum.STRUCTURE.V) {
      fetchDict()
    }
  },
  { immediate: true }
)

watch(
  () => props.defaultType,
  (val) => {
    if (val) {
      boardType.value = props.defaultType
    } else {
      boardType.value = props.showItem.length > 0 ? props.showItem[0] : undefined
    }
  },
  { deep: true, immediate: true }
)

watch(
  () => props.initForm,
  (val) => {
    if (isNotBlank(val)) {
      tableData.value = JSON.parse(JSON.stringify(val))
    }
  },
  { deep: true, immediate: true }
)

const currentView = computed(() => {
  switch (boardType.value) {
    case TechnologyTypeEnum.STRUCTURE.V: return structureTable
    case TechnologyTypeEnum.PROFILED_PLATE.V : return pressedColorTable
    case TechnologyTypeEnum.TRUSS_FLOOR_PLATE.V: return trussSupportTable
    case TechnologyTypeEnum.PRESSURE_BEARING_PLATE.V : return pressedSupportTable
    case TechnologyTypeEnum.SANDWICH_BOARD.V: return sandwichTable
    default: return ''
  }
})

const validateLength = (message, length) => {
  return [
    { required: true, message: message, trigger: ['change', 'blur'] },
    { max: length, message: `长度在 ${length} 个字符以内`, trigger: ['change', 'blur'] }
  ]
}

const validateThickness = (rule, value, callback) => {
  var reg = /^(?!(0[0-9]{0,}$))[0-9]{1,}[.]{0,}[0-9]{0,3}$/
  if (!value) {
    callback(new Error('请选择或填写厚度'))
  } else if (!reg.test(value)) {
    callback(new Error(`请输入数字且最多保留3位小数`))
  } else {
    callback()
  }
}

const validateWeight = (rule, value, callback) => {
  var reg = /^(?!(0[0-9]{0,}$))[0-9]{1,}[.]{0,}[0-9]{0,2}$/
  if (!value) {
    callback(new Error('请选择或填写容重'))
  } else if (!reg.test(value)) {
    callback(new Error(`请输入数字且最多保留2位小数`))
  } else {
    callback()
  }
}

const validateWidth = (rule, value, callback) => {
  var reg = /^[+]{0,1}(\d+)$/
  if (!value) {
    callback(new Error('请选择或填写有效宽度'))
  } else if (!reg.test(value)) {
    callback(new Error(`请输入整数`))
  } else {
    callback()
  }
}

const FIELD_INFO = {
  // 结构
  [TechnologyTypeEnum.STRUCTURE.V]: [
    {
      type: '产品种类',
      fields: [
        { field: 'type', name: '产品种类', placeholder: '请选择产品种类', rules: [{ required: true, message: '请选择产品种类', trigger: ['change', 'blur'] }] }
      ]
    }
  ],
  // 压型彩板
  [TechnologyTypeEnum.PROFILED_PLATE.V]: [
    {
      type: '产品信息',
      fields: [
        { field: 'brand', name: '品牌', dict: 'brand', placeholder: '请选择或填写品牌', rules: validateLength('请选择或填写品牌', 10) },
        { field: 'thickness', name: '厚度', dict: 'thickness', placeholder: '请选择或填写厚度', rules: [
          { validator: validateThickness, trigger: ['change', 'blur'] }
        ], unit: 'mm', decimalPlace: 3 },
        { field: 'plateType', name: '板型', dict: 'plate_type', placeholder: '请选择或填写板型', rules: validateLength('请选择或填写板型', 10) },
        { field: 'plating', name: '镀层', dict: 'cladding', placeholder: '请选择或填写镀层', rules: validateLength('请选择或填写镀层', 20) },
        { field: 'colour', name: '颜色', dict: 'color', placeholder: '请选择或填写颜色', rules: validateLength('请选择或填写颜色', 10) },
        { field: 'coating', name: '涂层', dict: 'coating', placeholder: '请选择或填写涂层', rules: validateLength('请选择或填写涂层', 10) },
        { field: 'yieldStrength', name: '屈服强度', dict: 'yield_strength', placeholder: '请选择或填写屈服强度', rules: validateLength('请选择或填写屈服强度', 10) }
      ]
    }
  ],
  // 桁架楼承板
  [TechnologyTypeEnum.TRUSS_FLOOR_PLATE.V]: [
    {
      type: '编号',
      fields: [
        { field: 'serialNumber', name: '编号', placeholder: '请选择编号', rules: validateLength('请选择编号', 20) }
      ]
    }
  ],
  // 压型楼承板
  [TechnologyTypeEnum.PRESSURE_BEARING_PLATE.V]: [
    {
      type: '产品信息',
      fields: [
        { field: 'mode', name: '类型', dict: 'mode', placeholder: '请选择或填写类型', rules: validateLength('请选择或填写类型', 10) },
        { field: 'plateType', name: '板型', dict: 'plate_type', placeholder: '请选择或填写板型', rules: validateLength('请选择或填写板型', 10) },
        { field: 'thickness', name: '厚度', dict: 'thickness', placeholder: '请选择或填写厚度', rules: [
          { validator: validateThickness, trigger: ['change', 'blur'] }
        ], unit: 'mm', decimalPlace: 3 },
        { field: 'brand', name: '品牌', dict: 'brand', placeholder: '请选择或填写品牌', rules: validateLength('请选择或填写品牌', 10) },
        { field: 'plating', name: '镀层', dict: 'plating', placeholder: '请选择或填写镀层', rules: validateLength('请选择或填写镀层', 20) },
        { field: 'yieldStrength', name: '屈服强度', dict: 'yield_strength', placeholder: '请选择或填写屈服强度', rules: validateLength('请选择或填写屈服强度', 10) }
      ]
    }
  ],
  // 夹芯板
  [TechnologyTypeEnum.SANDWICH_BOARD.V]: [
    {
      type: '产品信息',
      fields: [
        // { field: 'brand', name: '品牌系列', dict: 'brand', placeholder: '请选择或填写品牌', rules: validateLength('请选择或填写品牌', 20) },
        { field: 'thickness', name: '厚度', dict: 'thickness', placeholder: '请选择或填写厚度', rules: [
          { validator: validateThickness, trigger: ['change', 'blur'] }
        ], unit: 'mm', decimalPlace: 3 },
        { field: 'plateType', name: '板型', dict: 'plate_type', placeholder: '请选择或填写板型', rules: validateLength('请选择或填写板型', 10) },
        { field: 'effectiveWidth', name: '有效宽度', dict: 'effective_width', placeholder: '请选择或填写有效宽度', rules: [
          { validator: validateWidth, trigger: ['change', 'blur'] }
        ], unit: 'mm', decimalPlace: 0 }
      ]
    },
    {
      type: '外板',
      fields: [
        { field: 'outMaterial', name: '材质', dict: 'out_material', placeholder: '请选择或填写材质', rules: validateLength('请选择或填写材质', 10) },
        { field: 'outCoating', name: '涂层', dict: 'out_coating', placeholder: '请选择或填写涂层', rules: validateLength('请选择或填写涂层', 10) },
        { field: 'outPlating', name: '镀层', dict: 'out_plating', placeholder: '请选择或填写镀层', rules: validateLength('请选择或填写镀层', 20) },
        { field: 'outSteelPlateBrand', name: '品牌', dict: 'out_steel_plate_brand', placeholder: '请选择或填写品牌', rules: validateLength('请选择或填写品牌', 20) },
        { field: 'outThickness', name: '厚度', dict: 'out_thickness', placeholder: '请选择或填写厚度', rules: [
          { validator: validateThickness, trigger: ['change', 'blur'] }
        ], unit: 'mm', decimalPlace: 3 },
        { field: 'outEffectiveWidth', name: '宽度', dict: 'out_effective_width', placeholder: '请选择或填写宽度', rules: [
          { validator: validateWidth, trigger: ['change', 'blur'] }
        ], unit: 'mm', decimalPlace: 0 },
        { field: 'outColour', name: '颜色', dict: 'out_colour', placeholder: '请选择或填写颜色', rules: validateLength('请选择或填写颜色', 10) },
        { field: 'outPlateShape', name: '板形状', dict: 'out_plate_shape', placeholder: '请选择或填写板形状', rules: validateLength('请选择或填写板形状', 10) }
      ]
    },
    {
      type: '内板',
      fields: [
        { field: 'intMaterial', name: '材质', dict: 'in_material', placeholder: '请选择或填写材质', rules: validateLength('请选择或填写材质', 10) },
        { field: 'intCoating', name: '涂层', dict: 'in_coating', placeholder: '请选择或填写涂层', rules: validateLength('请选择或填写涂层', 10) },
        { field: 'intPlating', name: '镀层', dict: 'in_plating', placeholder: '请选择或填写镀层', rules: validateLength('请选择或填写镀层', 20) },
        { field: 'intSteelPlateBrand', name: '品牌', dict: 'in_Steel_Plate_brand', placeholder: '请选择或填写品牌', rules: validateLength('请选择或填写品牌', 20) },
        { field: 'intThickness', name: '厚度', dict: 'in_thickness', placeholder: '请选择或填写厚度', rules: [
          { validator: validateThickness, trigger: ['change', 'blur'] }
        ], unit: 'mm', decimalPlace: 3 },
        { field: 'intEffectiveWidth', name: '宽度', dict: 'in_effective_width', placeholder: '请选择或填写宽度', rules: [
          { validator: validateWidth, trigger: ['change', 'blur'] }
        ], unit: 'mm', decimalPlace: 0 },
        { field: 'intColour', name: '颜色', dict: 'in_colour', placeholder: '请选择或填写颜色', rules: validateLength('请选择或填写颜色', 10) },
        { field: 'intPlateShape', name: '板形状', dict: 'in_plate_shape', placeholder: '请选择或填写板形状', rules: validateLength('请选择或填写板形状', 10) }
      ]
    },
    {
      type: '芯材',
      fields: [
        { field: 'typeName', name: '种类', dict: 'kind_core', placeholder: '请选择或填写种类', rules: validateLength('请选择或填写种类', 10) },
        { field: 'coreBrand', name: '品牌', dict: 'brand_core', placeholder: '请选择或填写品牌', rules: validateLength('请选择或填写品牌', 20) },
        { field: 'unitWeight', name: '容重', dict: 'unit_weight_core', placeholder: '请选择或填写容重', rules: [
          { validator: validateWeight, trigger: ['change', 'blur'] }
        ], unit: 'Kg/m³', decimalPlace: 2 }
      ]
    }
  ]
}

function plateTypeChange(val) {
  const choseVal = plateTypeData.value.find(v => v.plateType === val)
  if (choseVal) {
    form.value.effectiveWidth = choseVal.effectiveWidth
    form.value.unfoldedWidth = choseVal.unfoldedWidth
  }
}
function handleBlur(e, field, dictName) {
  const val = e.target.value
  form.value[field] = val
  // 储存 手动输入的值 项目保存的时候调用批量新增配置接口
  if (boardType.value !== TechnologyTypeEnum.STRUCTURE.V && boardType.value !== TechnologyTypeEnum.TRUSS_FLOOR_PLATE.V) {
    const labels = typeDict.value[dictName] && typeDict.value[dictName].map(v => v.label) || []
    if (!labels.includes(val) && val) {
      form.value.dict[dictName] = {
        name: dictName,
        label: val,
        type: boardType.value
      }
    } else {
      form.value.dict[dictName] = undefined
    }
  }
}
function addRow() {
  formRef.value.validate((valid) => {
    if (valid) {
      if (!isNotBlank(form.value)) {
        ElMessage({ message: '请填写数据', type: 'warning' })
        return
      }
      if (boardType.value === TechnologyTypeEnum.TRUSS_FLOOR_PLATE.V) {
        const trussVal = trussDict.value.find(k => k.code === form.value.serialNumber)
        if (trussVal) {
          trussVal.list.map(v => {
            form.value[v.name] = v.value
          })
        }
      }
      const row = Object.assign({}, form.value)
      tableData.value[boardType.value].push(row)
      reset()
    }
  })
}
function editRow(row) {
  if (isEditing.value) {
    addRow()
  }
  form.value = JSON.parse(JSON.stringify(row))
  isEditing.value = true
  if (!isNotBlank(form.value.dict)) {
    form.value.dict = {}
  }
}
async function fetchDict() {
  loading.value = true
  try {
    typeDict.value = {}
    trussDict.value = []
    const { content } = boardType.value !== TechnologyTypeEnum.STRUCTURE.V ? await getEnclosureDictList(boardType.value) : props.showCategory
    if (boardType.value !== TechnologyTypeEnum.TRUSS_FLOOR_PLATE.V && boardType.value !== TechnologyTypeEnum.STRUCTURE.V) {
      content.forEach(o => {
        typeDict.value[o.name] = o.labels
      })
    }
    if (boardType.value === TechnologyTypeEnum.PROFILED_PLATE.V || boardType.value === TechnologyTypeEnum.PRESSURE_BEARING_PLATE.V) {
      plateTypeData.value = content.find(v => v.name === 'plate_type')['plateTypeList'] || []
    } else {
      plateTypeData.value = []
    }
    trussDict.value = boardType.value === TechnologyTypeEnum.TRUSS_FLOOR_PLATE.V ? content : []
    loading.value = false
  } catch (error) {
    loading.value = false
    console.log(error, '获取技术交底配置')
  }
}
function reset() {
  form.value = {
    dict: {},
    effectiveWidth: undefined,
    unfoldedWidth: undefined
  }
  isEditing.value = false
  if (formRef.value) {
    formRef.value.resetFields()
  }
  if (formRef.value) {
    nextTick(() => {
      formRef.value.clearValidate()
    })
  }
}

defineExpose({
  tableData
})
</script>

<style lang='scss' scoped>
.form-title{
  font-weight: bold;
  font-size: 14px;
  color: #606266;
}

.form-row{
  display: flex;
  flex-wrap: wrap;
  margin-top: 10px;
}

::v-deep(.el-table .cell){
  padding: 0px;
}

::v-deep(.el-input-number .el-input__inner) {
  text-align: left;
}

::v-deep(.el-table thead.is-group th){
  background: #fff;
}
::v-deep(.enclosureWidth .el-form-item__content){
  width:100%;
}
</style>
