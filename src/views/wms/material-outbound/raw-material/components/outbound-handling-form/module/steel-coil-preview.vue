<template>
  <el-form
    v-if="unitLoaded"
    ref="formRef"
    :model="form"
    :rules="formRules"
    size="small"
    label-position="left"
    label-width="80px"
    style="display: flex; overflow: auto"
  >
    <el-card class="box-card">
      <template #header>
        <div class="card-header">
          <span>开平钢板</span>
        </div>
      </template>
      <div style="background:#eee;padding:10px;">
        <div style="display:flex;">
          <div style="min-width:200px;">
            <el-form-item label="规格" label-width="10">
              <span>Q355B</span>
            </el-form-item>
          </div>
          <div style="min-width:200px;">
            <el-form-item label="开平段数">
              8段
            </el-form-item>
          </div>
        </div>
        <div style="display:flex;">
          <div style="min-width:200px;">
            <el-form-item label="厚度" label-width="10">
              11.75 mm
            </el-form-item>
          </div>
          <div style="min-width:200px;">
            <el-form-item label="开平数量">
              870mm
            </el-form-item>
          </div>
        </div>
        <div style="display:flex;">
          <div style="min-width:200px;">
            <el-form-item label="宽度" label-width="10">
              870mm
            </el-form-item>
          </div>
          <div style="min-width:200px;">
            <el-form-item label="开平总重">
              34782.22kg
            </el-form-item>
          </div>
        </div>
      </div>
      <el-form-item prop="boolOutbound" label="是否出库" style="margin-top: 10px">
        <el-checkbox v-model="form.boolOutbound" size="large" />
      </el-form-item>
      <el-form-item prop="type" label="钢板类型" style="margin-top: 10px">
        <common-radio
          v-model="form.materialOutboundMode"
          :options="steelCoilOutboundModeEnum"
          type="enum"
          size="small"
          disabled
        />
      </el-form-item>
      <el-form-item prop="remark" label="所属项目" style="margin-top: 10px">
        <common-select
          v-model="form.projectId"
          :options="projectOptions"
          :dataStructure="{ key: 'id', label: 'name', value: 'id' }"
          clearable
          type="other"
          placeholder="所属项目"
          @change="handleProjectChange($event, $index, row)"
        />
      </el-form-item>
      <el-form-item prop="remark" label="区域" style="margin-top: 10px">
        <common-select
          v-model="form.projectId"
          :options="projectMap[form.projectId]?.children || []"
          :dataStructure="{ key: 'id', label: 'name', value: 'id' }"
          clearable
          type="other"
          placeholder="所属项目"
          @change="handleProjectChange($event, $index, row)"
        />
      </el-form-item>
      <el-form-item prop="remark" label="区域" style="margin-top: 10px">
        <common-select
          v-model="form.projectId"
          :options="monomerMap?.[form.monomerId]?.children || []"
          :dataStructure="{ key: 'id', label: 'name', value: 'id' }"
          clearable
          type="other"
          placeholder="所属项目"
          @change="handleProjectChange($event, $index, row)"
        />
      </el-form-item>
      <el-form-item prop="workshopId" label="领用车间" style="margin-top: 10px">
       <workshop-select v-model="form.workshopId" :factory-id="material.factory?.id" placeholder="可选择车间" style="width: 200px" clearable />
      </el-form-item>
      <el-form-item prop="recipientId" label="领用人" style="margin-top: 10px">
        <user-dept-cascader
          v-model="form.recipientId"
          :collapse-tags="false"
          clearable
          filterable
          show-all-levels
          placeholder="领用人"
          style="width: 200px;"
        />
      </el-form-item>
      <el-form-item prop="date" label="出库日期" style="margin-top: 10px">
        <el-date-picker
          v-model="form.date"
          type="date"
          value-format="x"
          placeholder="出库日期"
          style="width: 200px;"
        />
      </el-form-item>
    </el-card>
  </el-form>
</template>

<script setup>
import { steelCoilOutboundHandling } from '@/api/wms/material-outbound/raw-material/outbound-handling'
import { defineProps, reactive, defineExpose, provide, computed, ref, watch, watchEffect, defineEmits } from 'vue'
import { mapGetters } from '@/store/lib'
import { deepClone, isBlank, isNotBlank, toPrecision } from '@/utils/data-type'
import { calcSteelCoilWeight } from '@/utils/wms/measurement-calc'
import { positiveNumPattern } from '@/utils/validate/pattern'
// import { outboundDestinationTypeEnum, projectWarehouseTypeEnum } from '@/utils/enum/modules/wms'

import { projectNameFormatter } from '@/utils/project'
import { validate } from '@compos/form/use-table-validate'
import useMatBaseUnit from '@/composables/store/use-mat-base-unit'
import useTableValidate from '@compos/form/use-table-validate'
import useWatchFormValidate from '@/composables/form/use-watch-form-validate'
import { numFmtByUnit, numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { ElMessage } from 'element-plus'
// 废料定义，退库长度应大于废料
import useSteelMinLengthConfig from '@compos/store/use-steel-minlength-config'
import { convertUnits } from '@/utils/convert/unit'

import useProjectTree from '@compos/store/use-project-tree'
import workshopSelect from '@comp-mes/workshop-select'
import userDeptCascader from '@comp-base/user-dept-cascader.vue'

const { steelMinLengthConfig } = useSteelMinLengthConfig()

const steelCoilOutboundModeEnum = {
  BY_LENGTH: { L: '按长度出库', K: 'BY_LENGTH ', V: 1 << 0 },
  BY_PLATE: { L: '按条板出库', K: 'BY_PLATE', V: 1 << 1 }
}

const { projectTree, projectMap, monomerMap } = useProjectTree()

// 项目选择
const projectOptions = computed(() => {
  if (isNotBlank(projectTree.value)) {
    const arr = projectTree.value.map((p) => {
      return { id: p.id, name: p.id !== 'common' ? projectNameFormatter(p, { showSerialNumber: false }) : '公共库' }
    })
    arr.unshift({ id: 'common', name: '公共库' })
    return arr
  } else {
    return null
  }
})

const minLength = computed(() => {
  return steelMinLengthConfig.value?.steelPlateShortestSideMinLength ? convertUnits(steelMinLengthConfig.value?.steelPlateShortestSideMinLength, 'mm', 'm') : 0
})

const props = defineProps({
  basicClass: {
    // 基础分类
    type: Number
  },
  material: {
    // 物料出库信息
    type: Object
  },
  maxHeight: {
    type: Number
  },
  projectWarehouseType: {
    type: [Number, String],
    default: undefined
  }
})

const formRef = ref()
// 表单
const form = ref({
  list: []
})

// 当前分类基础单位
const { loaded: unitLoaded, baseUnit } = useMatBaseUnit(props.basicClass)

// 监听校验
useWatchFormValidate(formRef, form, ['quantity'])
// 当前用户
const { user } = mapGetters('user')
// 材料
const material = computed(() => props.material || {})
const lengthTable = ref([{}])
const submitList = ref([])

// const isPlateOut = computed(() => { return false })
const isPlateOut = computed(() => form.value.materialOutboundMode === steelCoilOutboundModeEnum.BY_PLATE.V)

const validateQuantity = (rule, value, callback) => {
  if (isBlank(value)) {
    return callback(new Error('请填写数量'))
  }
  if (value <= 0) {
    return callback(new Error('数量必须大于0'))
  }
  if (value > maxQuantity.value) {
    return callback(new Error('数量不可超过可操作数量'))
  }
  callback()
}

const rules = {
  outboundAddress: [{ required: true, message: '出库目的地', trigger: 'change' }],
  quantity: [{ required: true, validator: validateQuantity, trigger: 'blur' }],
  projectId: [{ required: true, message: '请选择出库项目', trigger: 'change' }],
  remark: [{ max: 200, message: '不能超过200个字符', trigger: 'blur' }]
}

const plateOutRules = {
  // singleQuantity: [{ required: true, validator: validateQuantity, trigger: 'blur' }],
  // segmentQuantity: [
  //   { required: true, message: '请填写段数', trigger: 'blur' },
  //   { pattern: positiveNumPattern, message: '数量必须大于0', trigger: 'blur' }
  // ],
  remark: [{ max: 200, message: '不能超过200个字符', trigger: 'blur' }]
}

// 提交校验
// function validatorWidth(value, row) {
//   console.log(value, row, props.material.width)
//   if (value > props.material.width) {
//     return false
//   }
//   return true
// }

// 提交校验
// function validatorLength(value, row) {
//   console.log(value, row, form.value.quantity)
//   if (value > form.value.quantity) {
//     return false
//   }
//   return true
// }

const tableRules = {
  width: [
    { required: true, message: '请填写宽度', trigger: 'blur' },
    // { validator: validatorWidth, message: '超出允许范围,不可提交', trigger: 'blr' },
    { pattern: positiveNumPattern, message: '宽度必须大于0', trigger: 'blur' }
  ],
  // length: [
  //   { required: true, message: '请填写长度', trigger: 'blur' },
  //   { validator: validatorLength, message: '超出允许范围,不可提交', trigger: 'blur' },
  //   { pattern: positiveNumPattern, message: '长度必须大于0', trigger: 'blur' }
  // ],
  quantity: [
    { required: true, message: '请填写数量', trigger: 'blur' },
    { pattern: positiveNumPattern, message: '数量必须大于0', trigger: 'blur' }
  ]
  // projectId: [{ required: true, message: '请选择出库项目', trigger: 'change' }]
}

const lengthTableRules = {
  singleQuantity: [
    { required: true, message: '请填写单段长度', trigger: 'blur' },
    // { validator: validatorWidth, message: '超出允许范围,不可提交', trigger: 'blr' },
    { pattern: positiveNumPattern, message: '单段长度必须大于0', trigger: 'blur' }
  ],
  segmentQuantity: [
    { required: true, message: '请填写段数', trigger: 'blur' },
    { pattern: positiveNumPattern, message: '段数必须大于0', trigger: 'blur' }
  ]
}

const { tableValidate, wrongCellMask } = useTableValidate({ rules: tableRules }) // 表格校验

const formRules = computed(() => {
  if (!isPlateOut.value) {
    return Object.assign({}, rules)
  } else {
    return Object.assign({}, plateOutRules)
  }
})

// 最大数量
const maxQuantity = computed(() => {
  if (!form.value || !form.value.projectId || !material.value.projectFrozenForUnitKV) return material.value.corOperableQuantity
  return material.value.corOperableQuantity + (material.value.projectFrozenForUnitKV[form.value.projectId] || 0)
})
provide('maxQuantity', maxQuantity)

// 余料
const surplusMaterial = computed(() => {
  let _width = material.value.width
  let _mete = form.value.singleTotalWeight
  form.value.list.forEach((item) => {
    _width -= item.width * (item.quantity || 1) || 0
    _mete -= item.mete || 0
  })
  return {
    width: _width,
    mete: _mete > 0 ? _mete : 0
  }
})

watch(
  material,
  (val) => {
    formInit(val)
  },
  { immediate: true }
)

watch(
  () => isPlateOut.value,
  (val) => {
    if (val) {
      const _row = rowInit()
      form.value.list = []
      form.value.list.push(_row)
    }
  },
  { immediate: true }
)

watch([() => form.value.singleQuantity, () => form.value.segmentQuantity], () => {
  if (isPlateOut.value) {
    form.value.quantity = toPrecision(
      form.value.singleQuantity && form.value.segmentQuantity ? form.value.singleQuantity * form.value.segmentQuantity : 0,
      material.value.outboundUnitPrecision
    )
  }
})

watch(
  lengthTable.value,
  (val) => {
    let totalLength = 0
    if (isNotBlank(val)) {
      val.forEach(v => {
        if (v.singleQuantity && v.segmentQuantity) {
          totalLength += (v.singleQuantity * v.segmentQuantity)
        }
      })
    }
    form.value.quantity = totalLength
    calTotalWeight()
  },
  { immediate: true, deep: true }
)

async function calTotalWeight() {
  const list = []
  const allArr = []
  form.value.totalWeight = 0
  if (isNotBlank(form.value.list) && isNotBlank(lengthTable.value)) {
    form.value.list.forEach(v => {
      lengthTable.value.forEach(k => {
        for (let i = 0; i < k.segmentQuantity; i++) {
          list.push({
            ...v,
            quantity: (v.quantity || 0),
            length: k.singleQuantity || 0
          })
        }
      })
    })
  }
  for (let i = 0; i < list.length; i++) {
    const row = list[i]
    row.mete = 0
    if (isNotBlank(row.quantity) && isNotBlank(row.length) && isNotBlank(row.width)) {
      const p = await calcSteelCoilWeight({
        name: row.name,
        length: row.length,
        width: row.width,
        thickness: row.thickness,
        quantity: row.quantity
      }).then((val) => {
        row.theoryWeight = val
        row.mete = row.mete || row.theoryWeight
      })
      if (p) allArr.push(p)
    }
  }
  await Promise.all(allArr)
  list.forEach(v => {
    v.boolSurplus = false
    v.boolOutbound = true
    form.value.totalWeight += (v.mete || 0)
  })
  submitList.value = list
}

function rowInit() {
  const _row = reactive({
    basicClass: props.basicClass,
    name: material.value.classifyFullName,
    thickness: material.value.thickness,
    // length: undefined,
    width: undefined,
    quantity: undefined,
    outboundAddress: outboundDestinationTypeEnum.FACTORY.V, // 出库目的地
    // projectId: form.value.list.length === 0 ? (props.projectWarehouseTypeEnum === projectWarehouseTypeEnum.PUBLIC.V ? 'common' : (material.value.project ? material.value.project.id : undefined)) : -1, // 项目id,
    // monomerId: form.value.list.length === 0 ? material.value?.monomerId : -1,
    // areaId: form.value.list.length === 0 ? material.value?.areaId : -1,
    // workshopId: form.value.list.length === 0 ? material.value?.workshop?.id : -1,
    overWidth: false,
    overLength: false
  })
  rowWatch(_row)
  return _row
}

function rowWatch(row) {
  // watchEffect(() => {
  //   row.overWidth = Boolean(row.width > material.value.width)
  //   row.overLength = Boolean(row.length > form.value.quantity)
  // })
  // 计算单件理论重量
  watch([() => row], () => {
    calTotalWeight()
    // calcMete(row)
  },
  { immediate: true, deep: true })
}

watchEffect(async () => {
  // form.value.totalWeight =
  //   toPrecision((form.value.quantity / maxQuantity.value) * material.value.operableMete, baseUnit.value?.weight?.precision) || 0
  // form.value.singleTotalWeight =
  //   toPrecision((form.value.singleQuantity / maxQuantity.value) * material.value.operableMete, baseUnit.value?.weight?.precision) || 0
  // form.value.singleTheoryWeight =
  //   (await calcSteelCoilWeight({
  //     name: material.value.classifyFullName,
  //     length: form.value.singleQuantity,
  //     width: material.value.width,
  //     thickness: material.value.thickness
  //   })) || 0
})
// 表单初始化
function formInit(data) {
  const newForm = {
    materialOutboundMode: steelCoilOutboundModeEnum.BY_LENGTH.V, // 钢卷出库方式
    materialId: data.id, // 物料id
    monomerId: data?.monomerId, // 单体id
    areaId: data?.areaId, // 区域id
    factoryId: data.factory?.id, // 车间id
    outboundUnit: data.outboundUnit, // 出库单位
    outboundUnitPrecision: data.outboundUnitPrecision, // 出库单位精度
    projectId: data.project ? data.project.id : undefined, // 项目id
    recipientId: user.value.id, // 领用人id
    // segmentQuantity: 1, // 段数
    // quantity: undefined, // 长度
    list: [],
    remark: undefined // 备注
  }
  form.value = newForm
}

function addLengthRow() {
  lengthTable.value.push({})
}

function checkTotalLength(row, key) {
  let totalLength = 0
  lengthTable.value.forEach(v => {
    if (v.singleQuantity && v.segmentQuantity) {
      totalLength += (v.singleQuantity * v.segmentQuantity)
    }
  })
  if (totalLength > maxQuantity.value) {
    row[key] = undefined
    ElMessage({ message: '开平总长不能大于可出库长度', type: 'error' })
  }
}

function lengthWrongCellMask({ row, column }) {
  if (!row) return
  const rules = lengthTableRules
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

// 设置最大数量
// function setMaxQuantity() {
//   form.value.quantity = maxQuantity.value
// }
function validateLengthTable() {
  if (lengthTable.value.length <= 0) {
    ElMessage({ message: '请先填写单段长度配置', type: 'error' })
    return false
  }
  const rules = lengthTableRules
  let flag = true
  lengthTable.value.map(row => {
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
  return true
}

async function nextSubmit() {
  const next = validateLengthTable()
  if (!next) return false
  const valid = await formRef.value.validate()
  if (!valid) return false
  const formData = deepClone(form.value)
  await numFmtByUnit(formData, {
    unit: formData.outboundUnit,
    precision: formData.outboundUnitPrecision,
    fields: ['quantity'],
    toSmallest: true,
    toNum: true
  })
  if (form.value.quantity > maxQuantity.value) {
    ElMessage.error(`开平总长不可大于可出库长度`)
    return false
  }
  let _width = 0
  form.value.list.forEach((v) => {
    _width += v.width * v.quantity
  })
  if (_width > material.value.width) {
    ElMessage.error(`条板总宽：${_width}mm，条板总宽不可大于开平宽度`)
    throw new Error('宽度超出允许值')
  }
  if (surplusMaterial.value?.width < 0) {
    ElMessage.error(`余料错误`)
    throw new Error('余料错误')
  }
  const { validResult } = tableValidate(form.value.list)
  if (!validResult) return false
  let _list = deepClone(submitList.value)
  const surplusMaterialList = []
  if (surplusMaterial.value?.width > 0) {
    for (let i = 0; i < lengthTable.value.length; i++) {
      surplusMaterialList.push({
        basicClass: props.basicClass,
        name: material.value.classifyFullName,
        thickness: material.value.thickness,
        width: surplusMaterial.value.width,
        length: lengthTable.value[i].singleQuantity,
        quantity: lengthTable.value[i].segmentQuantity,
        boolSurplus: true, // 是否余料
        boolOutbound: false
      })
    }
  }
  const allArr = []
  for (let i = 0; i < surplusMaterialList.length; i++) {
    const row = surplusMaterialList[i]
    row.mete = 0
    if (isNotBlank(row.quantity) && isNotBlank(row.length) && isNotBlank(row.width)) {
      const p = await calcSteelCoilWeight({
        name: row.name,
        length: row.length,
        width: row.width,
        thickness: row.thickness,
        quantity: row.quantity
      }).then((val) => {
        row.theoryWeight = val
        row.mete = row.mete || row.theoryWeight
      })
      if (p) allArr.push(p)
    }
  }
  await Promise.all(allArr)
  _list = [..._list, ...surplusMaterialList]
  return isNotBlank(_list) ? _list : undefined
}

// 出库办理，表单提交
async function submit() {
  const next = validateLengthTable()
  if (!next) return false
  const valid = await formRef.value.validate()
  if (!valid) return false
  const formData = deepClone(form.value)
  await numFmtByUnit(formData, {
    unit: formData.outboundUnit,
    precision: formData.outboundUnitPrecision,
    fields: ['quantity'],
    toSmallest: true,
    toNum: true
  })
  if (isPlateOut.value) {
    // let _weight = 0
    // if (form.value.quantity > maxQuantity.value) {
    //   ElMessage.error(`开平总长不可大于可出库长度`)
    //   return false
    // }
    // let _width = 0
    // form.value.list.forEach((v) => {
    //   _width += v.width * v.quantity
    // })
    // if (_weight > form.value.totalWeight) {
    //   ElMessage.error(`条板总重：${_weight}kg，条板总重不可大于开平总重`)
    //   throw new Error('重量超出允许值')
    // }
    // if (_width > material.value.width) {
    //   ElMessage.error(`条板总宽：${_width}mm，条板总宽不可大于开平宽度`)
    //   throw new Error('宽度超出允许值')
    // }
    // if (surplusMaterial.value?.width < 0) {
    //   ElMessage.error(`余料错误`)
    //   throw new Error('余料错误')
    // }
    // const { validResult } = tableValidate(form.value.list)
    // if (!validResult) return false
    // let _list = deepClone(submitList.value)
    // const surplusMaterialList = []
    // if (surplusMaterial.value?.width > 0) {
    //   for (let i = 0; i < lengthTable.value.length; i++) {
    //     surplusMaterialList.push({
    //       basicClass: props.basicClass,
    //       name: material.value.classifyFullName,
    //       thickness: material.value.thickness,
    //       width: surplusMaterial.value.width,
    //       length: lengthTable.value[i].singleQuantity,
    //       quantity: lengthTable.value[i].segmentQuantity,
    //       boolSurplus: true // 是否余料
    //     })
    //   }
    // }
    // const allArr = []
    // for (let i = 0; i < surplusMaterialList.length; i++) {
    //   const row = surplusMaterialList[i]
    //   row.mete = 0
    //   if (isNotBlank(row.quantity) && isNotBlank(row.length) && isNotBlank(row.width)) {
    //     const p = await calcSteelCoilWeight({
    //       name: row.name,
    //       length: row.length,
    //       width: row.width,
    //       thickness: row.thickness,
    //       quantity: row.quantity
    //     }).then((val) => {
    //       row.theoryWeight = val
    //       row.mete = row.mete || row.theoryWeight
    //     })
    //     if (p) allArr.push(p)
    //   }
    // }
    // await Promise.all(allArr)
    // _list = [..._list, ...surplusMaterialList]
    formData.battenList = await numFmtByBasicClass(formData.battenList, { toSmallest: true, toNum: true }, { weight: ['mete'] })
  }
  const res = await steelCoilOutboundHandling(formData)
  return res
}

// 重置表单
function resetForm() {
  formRef.value.resetFields()
}

// 清空校验
function clearValidate() {
  formRef.value && formRef.value.clearValidate()
}

function getSummaries(param) {
  const { columns, data } = param
  const sums = []
  columns.forEach((column, index) => {
    if (index === 0) {
      sums[index] = '余料'
      return
    }
    if (column.property === 'width') {
      let _width = material.value.width
      data.forEach((item) => {
        _width -= item.width * (item.quantity || 1) || 0
      })
      sums[index] = _width
    }
    if (column.property === 'mete') {
      let _mete = form.value.singleTotalWeight
      data.forEach((item) => {
        _mete -= item.mete || 0
      })
      sums[index] = _mete > 0 ? _mete : 0
    }
    if (column.property === 'quantity') {
      sums[index] = 1
    }
  })
  return sums
}

defineExpose({
  submit,
  nextSubmit,
  resetForm,
  clearValidate,
  enlargeWth: computed(() => isPlateOut.value)
})
</script>

<style lang="scss" scoped>
.set-title {
  font-weight: bold;
  font-size: 16px;
}
.tip {
  display: inline-block;
  color: red;
  margin-left: 15px;
}
.form {
  display: flex;
  flex-direction: row;
  justify-content: flex-start;
  align-items: flex-start;
}
.material-info {
  flex: auto;
}
.form-info {
  margin-left: 20px;
  width: 380px;
  flex: none;
}

.divider {
  display: block;
  height: 1px;
  width: 100%;
  margin: 20px 0;
  border-top: 1px dashed #e9e9e9;
}

.preview-info {
  position: relative;
  width: 100%;
  padding: 0 50px 50px 0;

  .plate-item {
    width: 100%;
    padding: 0 10px;
    box-sizing: border-box;
    background-color: #949090;
    color: #fff;
  }
}

.total-info {
  margin-left: 20px;

  .total-item {
    width: 150px;
    display: flex;
    flex-direction: column;
    align-items: center;
    justify-content: center;
    border: 1px solid #36ae81;
    border-radius: 5px;

    &:not(:last-child) {
      margin-bottom: 20px;
    }

    .total-label {
      background-color: #36ae81;
      color: #fff;
      height: 30px;
      width: 100%;
      text-align: center;
      line-height: 30px;
    }

    .total-value {
      height: 30px;
      width: 100%;
      text-align: center;
      line-height: 30px;
    }
  }

  .total-item-surplus {
    border-color: #f78230;
    .total-label {
      background-color: #f78230;
    }
  }
}

.other-info {
  // display: flex;
}
</style>
