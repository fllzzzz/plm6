<template>
  <el-form v-if="unitLoaded" ref="formRef" :model="form" :rules="formRules" size="small" label-position="left" label-width="120px">
    <el-form-item label="出库方式" class="material-outbound-mode-info">
      <common-radio v-model="form.materialOutboundMode" :options="steelCoilOutboundModeEnum" type="enum" size="small" />
    </el-form-item>
    <div :class="isPlateOut ? 'plate-out-form' : 'form'">
      <div :class="isPlateOut ? 'plate-out-material-info' : 'material-info'">
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
      <div class="form-info" v-if="!isPlateOut">
        <common-form-item :material="material" :form="form" />
      </div>
      <div class="plate-out-material-info" style="margin-top: 20px" v-else>
        <el-form-item :label="`出库总长度(${material.outboundUnit})`" prop="quantity">
          <common-input-number
            v-model="form.quantity"
            :min="0"
            :precision="material.outboundUnitPrecision"
            :max="maxQuantity"
            controls-position="right"
          />
          <span class="text-clickable set-max-text" @click="setMaxQuantity" style="margin-left: 5px">全部出库</span>
        </el-form-item>
        <el-form-item :label="`开平总重（${baseUnit.weight.unit}）`">
          <span>{{ form.totalWeight || 0 }}</span>
        </el-form-item>
        <el-form-item :label="`剩余总长度(${material.outboundUnit})`">
          <span>{{ surplusQuantity || 0 }}</span>
        </el-form-item>
        <el-form-item :label="`剩余重量（${baseUnit.weight.unit}）`">
          <span>{{ surplusWeight || 0 }}</span>
        </el-form-item>
      </div>
      <template v-if="isPlateOut">
        <common-table
          :data="form.list"
          :max-height="maxHeight"
          :cell-class-name="wrongCellMask"
          style="width: 100%; margin-top: 20px; margin-bottom: 20px"
          return-source-data
          :show-empty-symbol="false"
        >
          <el-table-column label="序号" type="index" align="center" width="60" />
          <el-table-column prop="width" align="center" width="135px" :label="`宽 (${baseUnit.width.unit})`">
            <template #default="{ row }">
              <el-tooltip
                class="item"
                effect="dark"
                content="宽度不可大于物料本身宽度"
                :disabled="!(row.width > material.width)"
                placement="top"
              >
                <common-input-number
                  v-model="row.width"
                  :min="0"
                  :max="999999999"
                  controls-position="right"
                  :controls="false"
                  :class="{ 'over-weight-tip': !!(row.width > material.width) }"
                  :precision="baseUnit.width.precision"
                  size="mini"
                  placeholder="宽"
                />
              </el-tooltip>
            </template>
          </el-table-column>
          <el-table-column prop="length" align="center" width="135px" :label="`长 (${baseUnit.length.unit})`">
            <template #default="{ row }">
              <el-tooltip
                class="item"
                effect="dark"
                content="长度不可大于出库总长度"
                :disabled="!(row.length > form.quantity)"
                placement="top"
              >
                <common-input-number
                  v-model="row.length"
                  :min="0"
                  :max="999999999"
                  :controls="false"
                  :precision="baseUnit.length.precision"
                  :class="{ 'over-weight-tip': !!(row.length > form.quantity) }"
                  size="mini"
                  placeholder="长"
                />
              </el-tooltip>
            </template>
          </el-table-column>
          <el-table-column prop="quantity" align="center" width="135px" label="数量 (张)">
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
          <el-table-column key="mete" prop="mete" align="center" :label="`总重 (${baseUnit.weight.unit})`" width="135px">
            <template #default="{ row }">
              <span>{{ row.mete || 0 }}</span>
            </template>
          </el-table-column>
          <!-- 项目设置 -->
          <project-set-columns :form="form" />
          <el-table-column label="操作" width="100px" align="center" fixed="right">
            <template #default="{ $index }">
              <common-button
                v-show="!(form.list.length === 1)"
                icon="el-icon-delete"
                type="danger"
                size="mini"
                class="icon-button"
                @click="delRow($index)"
              />
              <common-button
                v-show="$index === form.list.length - 1"
                icon="el-icon-plus"
                type="success"
                size="mini"
                class="icon-button"
                @click="addRow"
              />
            </template>
          </el-table-column>
        </common-table>
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
        <el-form-item label="备注" prop="remark">
          <el-input
            v-model.trim="form.remark"
            type="textarea"
            :autosize="{ minRows: 3, maxRows: 3 }"
            maxlength="200"
            show-word-limit
            placeholder="备注"
            style="width: 100%"
          />
        </el-form-item>
      </template>
    </div>
  </el-form>
</template>

<script setup>
import { steelCoilOutboundHandling } from '@/api/wms/material-outbound/raw-material/outbound-handling'
import { defineProps, reactive, defineExpose, provide, computed, ref, watch, watchEffect } from 'vue'
import { mapGetters } from '@/store/lib'
import { deepClone, isBlank, toPrecision } from '@/utils/data-type'
import { calcSteelCoilWeight } from '@/utils/wms/measurement-calc'
import { positiveNumPattern } from '@/utils/validate/pattern'

import useMatBaseUnit from '@/composables/store/use-mat-base-unit'
import useTableValidate from '@compos/form/use-table-validate'
import useWatchFormValidate from '@/composables/form/use-watch-form-validate'
import commonFormItem from '../components/common-form-item.vue'
import commonMaterialInfo from '../components/common-material-info.vue'
import { numFmtByUnit, numFmtByBasicClass } from '@/utils/wms/convert-unit'
import userDeptCascader from '@comp-base/user-dept-cascader.vue'
import projectSetColumns from '../components/project-set-columns.vue'
import { ElMessage } from 'element-plus'

const steelCoilOutboundModeEnum = {
  BY_LENGTH: { L: '按长度出库', K: 'BY_LENGTH ', V: 1 << 0 },
  BY_PLATE: { L: '按条板出库', K: 'BY_PLATE', V: 1 << 1 }
}

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
  quantity: [{ required: true, validator: validateQuantity, trigger: 'blur' }],
  // projectId: [{ required: true, message: '请选择出库项目', trigger: 'change' }],
  remark: [{ max: 200, message: '不能超过200个字符', trigger: 'blur' }]
}

// 提交校验
function validatorWidth(value, row) {
  console.log(value, row, props.material.width)
  if (value > props.material.width) {
    return false
  }
  return true
}

// 提交校验
function validatorLength(value, row) {
  console.log(value, row, form.value.quantity)
  if (value > form.value.quantity) {
    return false
  }
  return true
}

const tableRules = {
  width: [
    { required: true, message: '请填写宽度', trigger: 'blur' },
    { validator: validatorWidth, message: '超出允许范围,不可提交', trigger: 'blur' },
    { pattern: positiveNumPattern, message: '宽度必须大于0', trigger: 'blur' }
  ],
  length: [
    { required: true, message: '请填写长度', trigger: 'blur' },
    { validator: validatorLength, message: '超出允许范围,不可提交', trigger: 'blur' },
    { pattern: positiveNumPattern, message: '长度必须大于0', trigger: 'blur' }
  ],
  quantity: [
    { required: true, message: '请填写数量', trigger: 'blur' },
    { pattern: positiveNumPattern, message: '数量必须大于0', trigger: 'blur' }
  ],
  projectId: [{ required: true, message: '请选择出库项目', trigger: 'change' }]
}

const { tableValidate, wrongCellMask } = useTableValidate({ rules: tableRules }) // 表格校验

const formRules = computed(() => {
  let _rules = Object.assign({}, rules)
  if (!isPlateOut.value) {
    _rules = Object.assign(_rules, { projectId: [{ required: true, message: '请选择出库项目', trigger: 'change' }] })
  }
  return _rules
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
  return toPrecision(material.value.mete - form.value.totalWeight, baseUnit.value?.weight?.precision)
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

function rowInit() {
  const _row = reactive({
    basicClass: props.basicClass,
    name: material.value.classifyFullName,
    thickness: material.value.thickness,
    length: undefined,
    width: undefined,
    quantity: undefined,
    projectId: material.value.project ? material.value.project.id : undefined, // 项目id,
    monomerId: material.value?.monomerId,
    areaId: material.value?.areaId,
    workshopId: material.value?.workshop?.id
  })
  rowWatch(_row)
  return _row
}

function rowWatch(row) {
  console.log(row, 'row')
  // 计算单件理论重量
  watch([() => row.length, () => row.width, () => row.quantity], () => calcMete(row))
}

watchEffect(async () => {
  form.value.totalWeight =
    (await calcSteelCoilWeight({
      name: material.value.classifyFullName,
      length: form.value.quantity,
      width: material.value.width,
      thickness: material.value.thickness
    })) || 0
})

function addRow() {
  const _row = rowInit()
  form.value.list.push(_row)
}

// 删除行
function delRow(index) {
  form.value.list.splice(index, 1)
}

async function calcMete(row) {
  row.mete = await calcSteelCoilWeight({
    name: row.name,
    length: row.length,
    width: row.width,
    thickness: row.thickness,
    quantity: row.quantity
  })
}

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
    quantity: undefined, // 长度
    list: [],
    remark: undefined // 备注
  }
  form.value = newForm
}

// 设置最大数量
function setMaxQuantity() {
  form.value.quantity = maxQuantity.value
}

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
    let _weight = 0
    form.value.list.forEach((v) => {
      _weight += v.mete
    })
    if (_weight > form.value.totalWeight) {
      ElMessage.error(`条板总重：${_weight}kg，条板总重不可大于开平总重`)
      throw new Error('重量超出允许值')
    }
    const { validResult, dealList } = tableValidate(form.value.list)
    console.log(validResult, 'validResult')
    if (!validResult) throw new Error('表格请修正')
    formData.battenList = await numFmtByBasicClass(deepClone(dealList), { toSmallest: true, toNum: true }, { weight: ['mete'] })
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

defineExpose({
  submit,
  resetForm,
  clearValidate,
  enlargeWth: computed(() => isPlateOut.value)
})
</script>

<style lang="scss" scoped>
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

.plate-out-form {
  .plate-out-material-info {
    display: flex;
    flex-wrap: wrap;

    ::v-deep(.el-form-item) {
      width: 25%;
      margin-bottom: 5px;
    }
  }
}
</style>
