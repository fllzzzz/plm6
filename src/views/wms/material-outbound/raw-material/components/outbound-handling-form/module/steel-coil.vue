<template>
  <el-form
    v-if="unitLoaded"
    ref="formRef"
    :model="form"
    :rules="formRules"
    size="small"
    label-position="left"
    label-width="120px"
    style="display: flex; overflow: auto"
  >
    <div style="min-width: 800px">
      <el-form-item label="出库方式" class="material-outbound-mode-info">
        <template #label v-if="isPlateOut">
          <span class="set-title">出库方式</span>
        </template>
        <div style="display: flex">
          <common-radio
            v-model="form.materialOutboundMode"
            :options="steelCoilOutboundModeEnum"
            :unshowVal="
              material.classifyFullName.indexOf('卷板') === -1 || material.boolPartyA ? [steelCoilOutboundModeEnum.BY_PLATE.V] : []
            "
            type="enum"
            size="small"
            @change="materialOutboundModeChange"
          />
          <div class="tip" v-if="isPlateOut">
            <span>* 提示：</span>
            <span> 出库至钢板库后，无法再进行退库操作，请谨慎操作</span>
          </div>
        </div>
      </el-form-item>
      <div :class="isPlateOut ? 'plate-out-form' : 'form'">
        <div v-if="!isPlateOut" class="material-info">
          <common-material-info :material="material" :form="form">
            <template #afterSpec>
              <el-form-item label="厚 * 宽">
                <span>{{ `${material.thickness}${baseUnit.thickness.unit} * ${material.width}${baseUnit.width.unit}` }}</span>
              </el-form-item>
            </template>
            <template #afterBrand>
              <el-form-item label="炉批号">
                <span v-empty="{ val: material.heatNoAndBatchNo }" />
              </el-form-item>
            </template>
          </common-material-info>
        </div>
        <div v-if="!isPlateOut" class="form-info">
          <common-form-item :material="material" :form="form" />
        </div>
        <div v-if="isPlateOut" class="plate-out-material-info">
          <!-- <el-descriptions-item label="厚 * 宽">
              <span>{{ `${material.thickness}${baseUnit.thickness.unit} * ${material.width}${baseUnit.width.unit}` }}</span>
            </el-descriptions-item> -->
          <descriptions-material-info :material="material" :form="form">
            <template #afterSpec>
              <el-descriptions-item label="厚 * 宽">
                <span>{{ `${material.thickness}${baseUnit.thickness.unit} * ${material.width}${baseUnit.width.unit}` }}</span>
              </el-descriptions-item>
            </template>
            <template #afterBrand>
              <el-descriptions-item label="炉批号">
                <span v-empty="{ val: material.heatNoAndBatchNo }" />
              </el-descriptions-item>
            </template>
          </descriptions-material-info>
        </div>
        <template v-if="isPlateOut">
          <div class="divider"></div>
          <span class="set-title">单段配置</span>
          <common-table
            :data="form.list"
            :max-height="maxHeight"
            :cell-class-name="wrongCellMask"
            style="width: 100%; margin-top: 15px"
            show-summary
            :summary-method="getSummaries"
            return-source-data
            :show-empty-symbol="false"
          >
            <el-table-column label="序号" type="index" align="center" width="60" />
            <el-table-column prop="width" align="center" :label="`宽 (${baseUnit.width.unit})`" width="135">
              <template #default="{ row }">
                <!-- <el-tooltip class="item" effect="dark" content="宽度不可大于物料本身宽度" :disabled="!row.overWidth" placement="top"> -->
                <common-input-number
                  v-model="row.width"
                  :min="0"
                  :max="999999999"
                  controls-position="right"
                  :controls="false"
                  :class="{ 'over-weight-tip': row.overWidth }"
                  :precision="baseUnit.width.precision"
                  size="mini"
                  placeholder="宽"
                />
                <!-- </el-tooltip> -->
              </template>
            </el-table-column>
            <!-- <el-table-column prop="length" align="center" width="135px" :label="`长 (mm)`">
            <template #default>
              <el-tooltip class="item" effect="dark" content="长度不可大于出库总长度" :disabled="!row.overLength" placement="top">
                <common-input-number
                  v-model="row.length"
                  :min="0"
                  :max="999999999"
                  :controls="false"
                  :precision="baseUnit.length.precision"
                  :class="{ 'over-weight-tip': row.overLength }"
                  size="mini"
                  placeholder="长"
                />
              </el-tooltip>
              <span>{{ convertUnits(form.quantity, baseUnit.length.unit, 'mm') }}</span>
            </template>
          </el-table-column> -->
            <el-table-column prop="quantity" align="center" label="数量 (张)" width="135">
              <template #default="{ row }">
                <common-input-number
                  v-model="row.quantity"
                  :min="1"
                  :max="999999999"
                  controls-position="right"
                  :controls="false"
                  :step="1"
                  :precision="baseUnit.measure.precision"
                  size="mini"
                  placeholder="数量"
                />
              </template>
            </el-table-column>
            <el-table-column key="mete" prop="mete" align="center" :label="`总重 (${baseUnit.weight.unit})`" width="100">
              <template #default="{ row }">
                <span>{{ row.mete || 0 }}</span>
              </template>
            </el-table-column>
            <!-- 项目设置 -->
            <project-set-columns project-clearable :form="form" />
            <el-table-column label="操作" width="100px" align="center" fixed="right">
              <template #default="{ $index }">
                <common-button icon="el-icon-delete" type="danger" size="mini" class="icon-button" @click="delRow($index)" />
                <common-button icon="el-icon-plus" type="success" size="mini" class="icon-button" @click="addRow($index)" />
              </template>
            </el-table-column>
          </common-table>
          <div class="tip" style="margin-top:10px;" v-if="surplusMaterial.width < 0">
            <span>* 错误提示：</span>
            <span> 条板总宽不可大于开平宽度</span>
          </div>
        </template>
      </div>
    </div>
    <div v-if="isPlateOut" style="margin-left: 30px">
      <span class="set-title">单段开平预览（横向）</span>
      <div style="display: flex; margin-bottom: 15px; margin-top: 15px">
        <div class="preview-info" :style="`height:${previewHeight}px;`">
          <div
            v-if="
              form.singleQuantity && ((surplusMaterial.width && plateTotalQuantity > 1) || (!surplusMaterial.width && plateTotalQuantity))
            "
            :style="`height:${previewHeight}px;overflow:hidden;`"
          >
            <div v-for="(item, index) in form.list" :key="index">
              <template v-if="item.quantity && item.width">
                <div
                  v-for="x of item.quantity"
                  :key="x"
                  class="plate-item"
                  :style="{
                    marginBottom: index === form.list.length - 1 && !surplusMaterial.width ? '0px' : '1px',
                    height: ((previewHeight - (plateTotalQuantity - 1)) * item.width) / material.width + 'px',
                    lineHeight: ((previewHeight - (plateTotalQuantity - 1)) * item.width) / material.width + 'px',
                  }"
                >
                  {{ item.width }}{{ baseUnit.width.unit }}
                </div>
              </template>
            </div>
            <div
              v-if="surplusMaterial.width"
              class="plate-item"
              :style="{
                backgroundColor: '#c45656',
                height: ((previewHeight - (plateTotalQuantity - 1)) * surplusMaterial.width) / material.width + 'px',
                lineHeight: ((previewHeight - (plateTotalQuantity - 1)) * surplusMaterial.width) / material.width + 'px',
              }"
            >
              {{ surplusMaterial.width }}{{ baseUnit.width.unit }}
            </div>
          </div>
          <div
            v-else
            :style="`height:${previewHeight}px;line-height:${previewHeight}px;text-align:center;color: #9e9ea1;border: 1px solid;`"
          >
            <span>请配置单段长度和单段配置！</span>
          </div>
          <mark-size
            :sizeInfo="`${material.width}${baseUnit.width.unit}`"
            direction="vertical"
            :customStyle="`height:${previewHeight}px;right:-12px;top:0px;`"
          />
          <mark-size
            :sizeInfo="`${form.singleQuantity || 0}${material.outboundUnit}`"
            direction="horizontal"
            :customStyle="`width:320px;bottom:-30px;left:0px;`"
          />
        </div>
        <div class="total-info">
          <div class="total-item">
            <span class="total-label">开平总长</span>
            <span class="total-value">{{ form.quantity || 0 }} {{ material.outboundUnit }}</span>
          </div>
          <div class="total-item">
            <span class="total-label">开平总重</span>
            <span class="total-value">{{ form.totalWeight || 0 }} {{ baseUnit.weight.unit }}</span>
          </div>
          <div class="total-item total-item-surplus">
            <span class="total-label">剩余长度</span>
            <span class="total-value">{{ surplusQuantity || 0 }} {{ material.outboundUnit }}</span>
          </div>
          <div class="total-item total-item-surplus">
            <span class="total-label">剩余重量</span>
            <span class="total-value">{{ surplusWeight || 0 }} {{ baseUnit.weight.unit }}</span>
          </div>
        </div>
      </div>
      <div class="other-info">
        <div style="width: 300px">
          <el-form-item :label="`单段长度(${material.outboundUnit})`" prop="singleQuantity">
            <common-input-number
              v-model="form.singleQuantity"
              :min="0"
              :precision="material.outboundUnitPrecision"
              :max="maxQuantity"
              controls-position="right"
              style="width: 100%"
            />
          </el-form-item>
          <el-form-item :label="`段数(段)`" prop="segmentQuantity">
            <common-input-number
              v-model="form.segmentQuantity"
              :min="1"
              :precision="0"
              :max="form.singleQuantity ? parseInt(maxQuantity / form.singleQuantity) : 1"
              controls-position="right"
              style="width: 100%"
            />
          </el-form-item>
          <el-form-item label="领用人" prop="recipientId">
            <user-dept-cascader
              v-model="form.recipientId"
              :collapse-tags="false"
              clearable
              filterable
              show-all-levels
              placeholder="领用人"
              style="width: 100%"
            />
          </el-form-item>
        </div>
        <el-form-item prop="remark" label-width="0" style="margin-left: 20px">
          <el-input
            v-model.trim="form.remark"
            type="textarea"
            :autosize="{ minRows: 6, maxRows: 6 }"
            maxlength="200"
            show-word-limit
            placeholder="备注"
            style="width: 100%"
          />
        </el-form-item>
      </div>
    </div>
  </el-form>
</template>

<script setup>
import { steelCoilOutboundHandling } from '@/api/wms/material-outbound/raw-material/outbound-handling'
import { defineProps, reactive, defineExpose, provide, computed, ref, watch, watchEffect } from 'vue'
import { mapGetters } from '@/store/lib'
import { deepClone, isBlank, isNotBlank, toPrecision } from '@/utils/data-type'
import { calcSteelCoilWeight } from '@/utils/wms/measurement-calc'
import { positiveNumPattern } from '@/utils/validate/pattern'

import useMatBaseUnit from '@/composables/store/use-mat-base-unit'
import useTableValidate from '@compos/form/use-table-validate'
import useWatchFormValidate from '@/composables/form/use-watch-form-validate'
import commonFormItem from '../components/common-form-item.vue'
import commonMaterialInfo from '../components/common-material-info.vue'
import descriptionsMaterialInfo from '../components/descriptions-material-info'
import { numFmtByUnit, numFmtByBasicClass } from '@/utils/wms/convert-unit'
import userDeptCascader from '@comp-base/user-dept-cascader.vue'
import projectSetColumns from '../components/project-set-columns.vue'
import MarkSize from '@comp/MarkSize/index.vue'
import { ElMessage } from 'element-plus'
// import { convertUnits } from '@/utils/convert/unit'

const steelCoilOutboundModeEnum = {
  BY_LENGTH: { L: '按长度出库', K: 'BY_LENGTH ', V: 1 << 0 },
  BY_PLATE: { L: '按条板出库', K: 'BY_PLATE', V: 1 << 1 }
}

const previewHeight = 260

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
  }
})

const formRef = ref()
// 表单
const form = ref({})

// 当前分类基础单位
const { loaded: unitLoaded, baseUnit } = useMatBaseUnit(props.basicClass)

// 监听校验
useWatchFormValidate(formRef, form, ['quantity'])
// 当前用户
const { user } = mapGetters('user')
// 材料
const material = computed(() => props.material || {})

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
  projectId: [{ required: true, message: '请选择出库项目', trigger: 'change' }],
  workshopId: [{ required: true, message: '请选择出库车间', trigger: 'change' }],
  quantity: [{ required: true, validator: validateQuantity, trigger: 'blur' }],
  remark: [{ max: 200, message: '不能超过200个字符', trigger: 'blur' }]
}

const plateOutRules = {
  singleQuantity: [{ required: true, validator: validateQuantity, trigger: 'blur' }],
  segmentQuantity: [
    { required: true, message: '请填写段数', trigger: 'blur' },
    { pattern: positiveNumPattern, message: '数量必须大于0', trigger: 'blur' }
  ],
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

const surplusQuantity = computed(() => {
  return toPrecision(maxQuantity.value - form.value.quantity, material.value.outboundUnitPrecision)
})

const surplusWeight = computed(() => {
  return form.value.quantity === maxQuantity.value
    ? 0
    : toPrecision(material.value.operableMete - form.value.totalWeight, baseUnit.value?.weight?.precision)
})

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

// 开平总数
const plateTotalQuantity = computed(() => {
  let _total = 0
  form.value.list.forEach((item) => {
    _total += item.quantity || 0
  })
  if (surplusMaterial.value.width > 0) {
    _total += 1
  }
  return _total
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

function materialOutboundModeChange() {
  form.value.quantity = undefined
}

function rowInit() {
  const _row = reactive({
    basicClass: props.basicClass,
    name: material.value.classifyFullName,
    thickness: material.value.thickness,
    // length: undefined,
    width: undefined,
    quantity: undefined,
    // projectId: material.value.project ? material.value.project.id : undefined, // 项目id,
    monomerId: material.value?.monomerId,
    areaId: material.value?.areaId,
    workshopId: material.value?.workshop?.id,
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
  watch([() => form.value.singleQuantity, () => row.width, () => row.quantity], () => {
    calcMete(row)
  })
}

watchEffect(async () => {
  form.value.totalWeight =
    toPrecision((form.value.quantity / maxQuantity.value) * material.value.operableMete, baseUnit.value?.weight?.precision) || 0
  form.value.singleTotalWeight =
    toPrecision((form.value.singleQuantity / maxQuantity.value) * material.value.operableMete, baseUnit.value?.weight?.precision) || 0
  form.value.singleTheoryWeight =
    (await calcSteelCoilWeight({
      name: material.value.classifyFullName,
      length: form.value.singleQuantity,
      width: material.value.width,
      thickness: material.value.thickness
    })) || 0
})

function addRow(index) {
  const _row = rowInit()
  form.value.list.splice(index + 1, 0, _row)
}

// 删除行
function delRow(index) {
  form.value.list.splice(index, 1)
}

async function calcMete(row) {
  if (isNotBlank(row.quantity) && isNotBlank(form.value.singleQuantity) && isNotBlank(row.width)) {
    row.theoryWeight = await calcSteelCoilWeight({
      name: row.name,
      length: form.value.singleQuantity,
      width: row.width,
      thickness: row.thickness,
      quantity: row.quantity
    })
    row.mete = toPrecision(
      (row.theoryWeight / form.value.singleTheoryWeight) * form.value.singleTotalWeight,
      baseUnit.value?.weight?.precision
    )
  } else {
    row.mete = 0
  }
}

// 表单初始化
function formInit(data) {
  const newForm = {
    materialOutboundMode: steelCoilOutboundModeEnum.BY_LENGTH.V, // 钢卷出库方式
    materialId: data.id, // 物料id
    monomerId: data?.monomerId, // 单体id
    areaId: data?.areaId, // 区域id
    workshopId: data.workshop?.id, // 车间id
    outboundUnit: data.outboundUnit, // 出库单位
    outboundUnitPrecision: data.outboundUnitPrecision, // 出库单位精度
    projectId: data.project ? data.project.id : undefined, // 项目id
    recipientId: user.value.id, // 领用人id
    segmentQuantity: 1, // 段数
    quantity: undefined, // 长度
    list: [],
    remark: undefined // 备注
  }
  form.value = newForm
}

// 设置最大数量
// function setMaxQuantity() {
//   form.value.quantity = maxQuantity.value
// }

// 出库办理，表单提交
async function submit() {
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
    let _width = 0
    form.value.list.forEach((v) => {
      // _weight += v.mete
      _width += v.width * v.quantity
      v.length = form.value.singleQuantity
    })
    // if (_weight > form.value.totalWeight) {
    //   ElMessage.error(`条板总重：${_weight}kg，条板总重不可大于开平总重`)
    //   throw new Error('重量超出允许值')
    // }
    if (_width > material.value.width) {
      ElMessage.error(`条板总宽：${_width}mm，条板总宽不可大于开平宽度`)
      throw new Error('宽度超出允许值')
    }
    if (surplusMaterial.value?.width < 0) {
      ElMessage.error(`余料错误`)
      throw new Error('余料错误')
    }
    const { validResult, dealList } = tableValidate(form.value.list)
    const _list = deepClone(dealList)
    _list.forEach((v) => {
      v.quantity = v.quantity * form.value.segmentQuantity
      v.boolSurplus = false
    })
    if (surplusMaterial.value?.width > 0) {
      _list.push({
        basicClass: props.basicClass,
        name: material.value.classifyFullName,
        thickness: material.value.thickness,
        width: surplusMaterial.value.width,
        length: form.value.singleQuantity,
        quantity: form.value.segmentQuantity,
        mete: surplusMaterial.value.mete,
        boolSurplus: true // 是否余料
      })
    }
    // console.log(validResult, 'validResult')
    if (!validResult) throw new Error('表格请修正')
    formData.battenList = await numFmtByBasicClass(_list, { toSmallest: true, toNum: true }, { weight: ['mete'] })
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
  width: 370px;
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
    width: 120px;
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
  display: flex;
}
</style>
