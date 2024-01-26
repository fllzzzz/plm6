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
      <div :class="isPlateOut ? 'plate-out-form' : 'form'">
        <div v-if="isPlateOut" class="plate-out-material-info">
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
            :max-height="280"
            :cell-class-name="wrongCellMask"
            style="width: 100%; margin-top: 15px"
            show-summary
            :summary-method="getSummaries"
            return-source-data
            :show-empty-symbol="false"
          >
            <el-table-column label="序号" type="index" align="center" width="60" />
            <el-table-column prop="width" align="center" :label="`宽 (${baseUnit.width.unit})`">
              <template #default="{ row }">
                <common-input-number
                  v-model="row.width"
                  :min="minLength"
                  :max="999999999"
                  controls-position="right"
                  :controls="false"
                  :class="{ 'over-weight-tip': row.overWidth }"
                  :precision="baseUnit.width.precision"
                  size="mini"
                  placeholder="宽"
                />
              </template>
            </el-table-column>
            <el-table-column prop="quantity" align="center" label="数量 (张)">
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
            <!-- 项目设置 -->
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
          <div>
          <span class="set-title" style="margin-top:10px;">单段开平预览（横向）</span>
          <div style="display: flex; margin-bottom: 15px; margin-top: 15px">
            <div class="preview-info" :style="`height:${previewHeight}px;`">
              <div
                v-if="
                  ((surplusMaterial.width && plateTotalQuantity > 1) || (!surplusMaterial.width && plateTotalQuantity))
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
                <span>请配置单段配置！</span>
              </div>
              <mark-size
                :sizeInfo="`${material.width}${baseUnit.width.unit}`"
                direction="vertical"
                :customStyle="`height:${previewHeight}px;right:-12px;top:0px;`"
              />
              <mark-size
                :sizeInfo="`${form.singleQuantity || 0}${material.outboundUnit}`"
                direction="horizontal"
                :showType="'unshow'"
                :customStyle="`width:100%;bottom:-30px;left:0px;`"
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
          </div>
        </template>
      </div>
    </div>
    <div v-if="isPlateOut" style="margin-left: 30px">
      <div class="other-info">
        <div style="width: 530px">
          <div style="margin-bottom:10px;"><span class="set-title">单段长度配置</span> <common-button icon="el-icon-plus" style="float:right;" type="success" size="mini" class="icon-button" @click="addLengthRow" /></div>
          <common-table :data="lengthTable" :max-height="maxHeight-150" :cell-class-name="lengthWrongCellMask" return-source-data :show-empty-symbol="false">
            <el-table-column label="序号" type="index" align="center" width="55" />
            <el-table-column prop="singleQuantity" align="center" :label="`单段长度(${material.outboundUnit})`" min-width="120">
              <template #default="{ row }">
                <common-input-number
                  v-model="row.singleQuantity"
                  :min="minLength"
                  :max="maxQuantity"
                  controls-position="right"
                  :controls="false"
                  :precision="material.outboundUnitPrecision"
                  size="mini"
                  placeholder="单段长度"
                  @blur="checkTotalLength(row,'singleQuantity')"
                />
              </template>
            </el-table-column>
            <el-table-column prop="segmentQuantity" align="center" :label="`段数(段)`" min-width="120">
              <template #default="{ row }">
                <common-input-number
                  v-model="row.segmentQuantity"
                  :min="0"
                  :max="row.singleQuantity ? parseInt(maxQuantity / row.singleQuantity) : 1"
                  controls-position="right"
                  :controls="false"
                  :precision="0"
                  size="mini"
                  placeholder="段数(段)"
                  @blur="checkTotalLength(row,'segmentQuantity')"
                />
              </template>
            </el-table-column>
            <el-table-column label="操作" width="60px" align="center" fixed="right">
              <template #default="{ $index }">
                <common-button icon="el-icon-delete" type="danger" size="mini" class="icon-button" @click="delLengthRow($index)" />
              </template>
            </el-table-column>
          </common-table>
          <el-form-item prop="remark" label-width="0" style="margin-top: 10px">
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
    </div>
  </el-form>
</template>

<script setup>
import { steelCoilOutboundHandling } from '@/api/wms/material-outbound/raw-material/outbound-handling'
import { defineProps, reactive, defineExpose, provide, computed, ref, watch, watchEffect } from 'vue'
// import { mapGetters } from '@/store/lib'
import { deepClone, isBlank, isNotBlank, toPrecision } from '@/utils/data-type'
import { calcSteelCoilWeight } from '@/utils/wms/measurement-calc'
import { positiveNumPattern } from '@/utils/validate/pattern'
import { outboundDestinationTypeEnum } from '@/utils/enum/modules/wms'
// import { outboundDestinationTypeEnum, projectWarehouseTypeEnum } from '@/utils/enum/modules/wms'

import { validate } from '@compos/form/use-table-validate'
import useMatBaseUnit from '@/composables/store/use-mat-base-unit'
import useTableValidate from '@compos/form/use-table-validate'
import useWatchFormValidate from '@/composables/form/use-watch-form-validate'
import descriptionsMaterialInfo from '../components/descriptions-material-info'
import { numFmtByUnit, numFmtByBasicClass } from '@/utils/wms/convert-unit'
// import userDeptCascader from '@comp-base/user-dept-cascader.vue'
// import projectSetColumns from '../components/project-set-columns.vue'
import MarkSize from '@comp/MarkSize/index.vue'
import { ElMessage } from 'element-plus'
// 废料定义，退库长度应大于废料
import useSteelMinLengthConfig from '@compos/store/use-steel-minlength-config'
import { convertUnits } from '@/utils/convert/unit'

const { steelMinLengthConfig } = useSteelMinLengthConfig()

const steelCoilOutboundModeEnum = {
  BY_LENGTH: { L: '按长度出库', K: 'BY_LENGTH ', V: 1 << 0 },
  BY_PLATE: { L: '按条板出库', K: 'BY_PLATE', V: 1 << 1 }
}

const previewHeight = 260

const minLength = computed(() => {
  return steelMinLengthConfig.value?.steelPlateShortestSideMinLength ? steelMinLengthConfig.value?.steelPlateShortestSideMinLength + 1 : 0
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
  },
  formData: {
    type: Object,
    default: () => {}
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
// const { user } = mapGetters('user')
// 材料
const material = computed(() => {
  const obj = JSON.parse(JSON.stringify(props.material)) || {}
  obj.corQuantity = convertUnits(obj.corQuantity, 'm', 'mm')
  obj.corOperableQuantity = convertUnits(obj.corOperableQuantity, 'm', 'mm')
  if (obj.projectFrozenForUnitKV?.[form.value.projectId]) {
    obj.projectFrozenForUnitKV[form.value.projectId] = convertUnits(obj.projectFrozenForUnitKV[form.value.projectId], 'm', 'mm')
  }
  obj.outboundUnit = 'mm'
  obj.outboundUnitPrecision = 0
  return obj
})
const lengthTable = ref([{}])
const submitList = ref([])

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
  remark: [{ max: 200, message: '不能超过200个字符', trigger: 'blur' }]
}

const tableRules = {
  width: [
    { required: true, message: '请填写宽度', trigger: 'blur' },
    { pattern: positiveNumPattern, message: '宽度必须大于0', trigger: 'blur' }
  ],
  quantity: [
    { required: true, message: '请填写数量', trigger: 'blur' },
    { pattern: positiveNumPattern, message: '数量必须大于0', trigger: 'blur' }
  ]
}

const lengthTableRules = {
  singleQuantity: [
    { required: true, message: '请填写单段长度', trigger: 'blur' },
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
    const _row = rowInit()
    form.value.list = []
    form.value.list.push(_row)
  },
  { immediate: true, deep: true }
)

// watch([() => form.value.singleQuantity, () => form.value.segmentQuantity], () => {
//   if (isPlateOut.value) {
//     form.value.quantity = toPrecision(
//       form.value.singleQuantity && form.value.segmentQuantity ? form.value.singleQuantity * form.value.segmentQuantity : 0,
//       material.value.outboundUnitPrecision
//     )
//   }
// })

// watch(
//   lengthTable.value,
//   (val) => {
//     let totalLength = 0
//     if (isNotBlank(val)) {
//       val.forEach(v => {
//         if (v.singleQuantity && v.segmentQuantity) {
//           totalLength += (v.singleQuantity * v.segmentQuantity)
//         }
//       })
//     }
//     form.value.quantity = totalLength
//     calTotalWeight()
//   },
//   { immediate: true, deep: true }
// )

async function calTotalWeight() {
  const list = []
  const allArr = []
  if (isNotBlank(form.value.list) && isNotBlank(lengthTable.value)) {
    form.value.list.forEach(v => {
      lengthTable.value.forEach(k => {
        for (let i = 0; i < k.segmentQuantity; i++) {
          list.push({
            ...v,
            quantity: (v.quantity || 0),
            length: k.singleQuantity || 0,
            mete: 0
          })
        }
      })
    })
  }
  for (let i = 0; i < list.length; i++) {
    const row = list[i]
    row.mete = 0
    row.length = row.length ? convertUnits(row.length, 'mm', 'm') : 0
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
  const surplusMaterialListArr = []
  if (surplusMaterial.value?.width > 0) {
    for (let i = 0; i < lengthTable.value.length; i++) {
      for (let k = 0; k < lengthTable.value[i].segmentQuantity; k++) {
        surplusMaterialListArr.push({
          basicClass: props.basicClass,
          name: material.value.classifyFullName,
          thickness: material.value.thickness,
          width: surplusMaterial.value.width,
          length: convertUnits(lengthTable.value[i].singleQuantity, 'mm', 'm'),
          quantity: 1,
          boolSurplus: true, // 是否余料
          boolOutbound: false
        })
      }
    }
  }
  const allArr1 = []
  for (let i = 0; i < surplusMaterialListArr.length; i++) {
    const row = surplusMaterialListArr[i]
    row.mete = 0
    if (isNotBlank(row.quantity) && isNotBlank(row.length) && isNotBlank(row.width)) {
      const p1 = await calcSteelCoilWeight({
        name: row.name,
        length: row.length,
        width: row.width,
        thickness: row.thickness,
        quantity: row.quantity
      }).then((val) => {
        row.theoryWeight = val
        row.mete = row.mete || row.theoryWeight
      })
      if (p1) allArr1.push(p1)
    }
  }
  await Promise.all(allArr)
  await Promise.all(allArr1)
  let totalWeight = 0
  let extraWeight = props.material.operableMete
  let extraLength = maxQuantity.value
  let extraWidth = props.material.width
  form.value.list.forEach(v => {
    extraWidth -= v.width
  })
  list.forEach(v => {
    v.boolSurplus = false
    v.boolOutbound = true
    const convertLength = convertUnits(v.length, 'm', 'mm')
    extraLength -= convertLength
    if (extraWeight > 0) {
      if (extraWeight > v.mete) {
        if (extraLength > 0) {
          extraWeight -= v.mete
        } else {
          if (extraWidth > 0) {
            extraWeight -= v.mete
          } else {
            v.mete = extraWeight
            extraWeight = 0
          }
        }
      } else {
        v.mete = extraWeight
        extraWeight = 0
      }
    } else {
      v.mete = 0
    }
    totalWeight += (v.mete || 0)
  })
  surplusMaterialListArr.forEach((v, index) => {
    if (extraWeight > 0) {
      if (extraWeight > v.mete) {
        if (extraLength > 0) {
          extraWeight -= v.mete
        } else {
          if ((index < surplusMaterialListArr.length - 1)) {
            extraWeight -= v.mete
          } else {
            v.mete = extraWeight
            extraWeight = 0
          }
        }
      } else {
        v.mete = extraWeight
        extraWeight = 0
      }
    } else {
      v.mete = 0
    }
    totalWeight += (v.mete || 0)
  })
  form.value.totalWeight = totalWeight
  submitList.value = list
}

function rowInit() {
  const _row = reactive({
    basicClass: props.basicClass,
    name: material.value.classifyFullName,
    thickness: material.value.thickness,
    width: undefined,
    quantity: undefined,
    outboundAddress: outboundDestinationTypeEnum.FACTORY.V, // 出库目的地
    overWidth: false,
    overLength: false
  })
  rowWatch(_row)
  return _row
}

function lengthRowInit() {
  const _row = reactive({
    singleQuantity: undefined,
    segmentQuantity: undefined
  })
  lengthWatch(_row)
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

function lengthWatch(row) {
  // 计算单件理论重量
  watch([() => row], () => {
    let totalLength = 0
    if (isNotBlank(lengthTable.value)) {
      lengthTable.value.forEach(v => {
        if (v.singleQuantity && v.segmentQuantity) {
          totalLength += (v.singleQuantity * v.segmentQuantity)
        }
      })
    }
    form.value.quantity = totalLength
    calTotalWeight()
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

function addRow(index) {
  const _row = rowInit()
  form.value.list.splice(index + 1, 0, _row)
}

// 删除行
function delRow(index) {
  form.value.list.splice(index, 1)
}

function delLengthRow(index) {
  lengthTable.value.splice(index, 1)
}

// 表单初始化
function formInit(data) {
  lengthTable.value = []
  const newForm = {
    materialOutboundMode: steelCoilOutboundModeEnum.BY_PLATE.V, // 钢卷出库方式
    materialId: data.id, // 物料id
    monomerId: data?.monomerId, // 单体id
    areaId: data?.areaId, // 区域id
    factoryId: data.factory?.id, // 车间id
    outboundUnit: data.outboundUnit, // 出库单位
    outboundUnitPrecision: data.outboundUnitPrecision, // 出库单位精度
    projectId: data.project ? data.project.id : undefined, // 项目id
    recipientId: undefined, // 领用人id
    // segmentQuantity: 1, // 段数
    // quantity: undefined, // 长度
    list: [],
    remark: undefined // 备注
  }
  form.value = newForm
}

function addLengthRow() {
  const _row = lengthRowInit()
  lengthTable.value.push(_row)
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

async function validateSubmit() {
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
      for (let k = 0; k < lengthTable.value[i].segmentQuantity; k++) {
        surplusMaterialList.push({
          basicClass: props.basicClass,
          name: material.value.classifyFullName,
          thickness: material.value.thickness,
          width: surplusMaterial.value.width,
          length: convertUnits(lengthTable.value[i].singleQuantity, 'mm', 'm'),
          quantity: 1,
          boolSurplus: true, // 是否余料
          boolOutbound: false
        })
      }
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
  let extraWeight = props.material.operableMete
  let extraLength = maxQuantity.value
  let extraWidth = props.material.width
  form.value.list.forEach(v => {
    extraWidth -= v.width
  })
  _list.forEach(v => {
    const convertLength = convertUnits(v.length, 'm', 'mm')
    extraLength -= convertLength
    if (extraWeight > 0) {
      if (extraWeight > v.mete) {
        if (extraLength > 0) {
          extraWeight -= v.mete
        } else {
          if (extraWidth > 0) {
            extraWeight -= v.mete
          } else {
            v.mete = extraWeight
            extraWeight = 0
          }
        }
      } else {
        v.mete = extraWeight
        extraWeight = 0
      }
    } else {
      v.mete = 0
    }
  })
  surplusMaterialList.forEach((v, index) => {
    if (extraWeight > 0) {
      if (extraWeight > v.mete) {
        if (extraLength > 0) {
          extraWeight -= v.mete
        } else {
          if ((index < surplusMaterialList.length - 1)) {
            extraWeight -= v.mete
          } else {
            v.mete = extraWeight
            extraWeight = 0
          }
        }
      } else {
        v.mete = extraWeight
        extraWeight = 0
      }
    } else {
      v.mete = 0
    }
  })
  _list = [..._list, ...surplusMaterialList]
  const data = JSON.parse(JSON.stringify(form.value))
  delete data.list
  data.list = _list
  Object.assign(props.formData, data)
  return true
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
  formInit(props.material)
  const _row = rowInit()
  form.value.list = []
  form.value.list.push(_row)
}

function getSummaries(param) {
  const { columns, data } = param
  const sums = []
  let _width = material.value.width
  data.forEach((item) => {
    _width -= item.width * (item.quantity || 1) || 0
  })
  columns.forEach((column, index) => {
    if (index === 0) {
      sums[index] = '余料'
      return
    }
    if (column.property === 'width') {
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
      let _width = material.value.width
      data.forEach((item) => {
        _width -= item.width * (item.quantity || 1) || 0
      })
      sums[index] = _width <= 0 ? 0 : 1
    }
  })
  return sums
}

defineExpose({
  submit,
  validateSubmit,
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
  // display: flex;
}
</style>
