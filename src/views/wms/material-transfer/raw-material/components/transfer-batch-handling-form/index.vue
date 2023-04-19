<template>
  <common-dialog
    title="调拨办理"
    v-model="dialogVisible"
    width="90%"
    :before-close="handleClose"
    :show-close="true"
    custom-class="wms-batch-transfer-handling"
    top="10vh"
  >
    <template #titleRight>
      <common-button :loading="submitLoading" size="mini" type="primary" @click="submit"> 提 交 </common-button>
    </template>
    <el-form ref="formRef" class="form" :model="form" :rules="rules" size="small" label-position="right" inline label-width="70px">
      <div class="form-header">
        <el-form-item label="类型" prop="transferType" label-width="55px">
          <common-radio
            v-model="form.transferType"
            :options="transferNormalTypeEnum.ENUM"
            :disabled-val="disabledTransferType"
            type="enum"
            size="small"
            @change="handleTypeChange"
          />
        </el-form-item>
        <template v-if="showProjectSelect">
            <el-form-item label="项目" prop="projectId" label-width="55px">
              <project-cascader
                v-model="form.projectId"
                clearable
                class="input-underline"
                style="width: 200px"
                @change="handleProjectChange"
              />
            </el-form-item>
            <el-form-item label="单体" prop="monomerId" label-width="55px">
              <common-select
                v-model="form.monomerId"
                :options="form.projectId && projectMap?.[form.projectId]?.children || []"
                :dataStructure="{ key: 'id', label: 'name', value: 'id' }"
                class="input-underline"
                clearable
                type="other"
                placeholder="可选择单体"
                style="width: 200px"
              />
            </el-form-item>
            <el-form-item label="区域" prop="areaId" label-width="55px">
              <common-select
                v-model="form.areaId"
                :options="form.monomerId && monomerMap?.[form.monomerId]?.children || []"
                :dataStructure="{ key: 'id', label: 'name', value: 'id' }"
                class="input-underline"
                clearable
                type="other"
                placeholder="可选择区域"
                style="width: 200px"
              />
            </el-form-item>
          </template>
        <el-form-item v-if="showWorkshopAndWare" label="车间" prop="workshopId" label-width="55px">
          <workshop-select v-model="form.workshopId" placeholder="车间" class="input-underline" style="width: 200px" />
        </el-form-item>
        <el-form-item v-if="showWorkshopAndWare" label="仓库" prop="warehouseId" label-width="55px">
          <warehouse-select
            v-model="form.warehouseId"
            :workshop-id="form.workshopId"
            :basic-class="basicClass"
            placeholder="存储位置"
            class="input-underline"
            style="width: 200px"
          />
        </el-form-item>
      </div>
      <common-table
        ref="tableRef"
        :data="form.list"
        :data-format="columnsDataFormat"
        :max-height="maxHeight"
        :default-expand-all="false"
        :expand-row-keys="expandRowKeys"
        row-key="id"
      >
        <el-expand-table-column :data="form.list" v-model:expand-row-keys="expandRowKeys" row-key="id" fixed="left">
          <template #default="{ row }">
            <expand-secondary-info :basic-class="row.basicClass" :row="row">
              <p>
                项目：<span>{{ row.projectFullName }}</span>
              </p>
              <el-input
                v-model="row.sourceRow.remark"
                :autosize="remarkTextSize"
                type="textarea"
                placeholder="备注"
                maxlength="200"
                show-word-limit
                style="max-width: 400px"
              />
            </expand-secondary-info>
          </template>
        </el-expand-table-column>
        <!-- 基础信息 -->
        <material-base-info-columns :basic-class="basicClass" fixed="left" />
        <!-- 单位及其数量 -->
        <material-unit-operate-quantity-columns :basic-class="basicClass" />
        <!-- 次要信息 -->
        <material-secondary-info-columns :basic-class="basicClass" :show-batch-no="false" />
        <warehouse-info-columns />
        <el-table-column label="调拨数量" width="170px" align="center" fixed="right">
          <template #header>
            <span>调拨数量</span>
            <span class="text-clickable" style="margin-left: 10px" @click="setMaxQuantity">全部调拨</span>
            <span class="text-clickable" style="margin-left: 10px" @click="clearQuantity">清空</span>
          </template>
          <template #default="{ row: { sourceRow: row } }">
            <span class="flex-rbc">
              <common-input-number
                v-model="row.batchTransferQuantity"
                :min="0"
                :precision="row.outboundUnitPrecision"
                :max="row.corOperableQuantity"
                controls-position="right"
              />
              <span style="flex: none; margin-left: 10px">{{ row.outboundUnit }}</span>
            </span>
          </template>
        </el-table-column>
      </common-table>
    </el-form>
  </common-dialog>
</template>

<script setup>
import {
  steelPlateBatchTransferHandling,
  sectionSteelBatchTransferHandling,
  steelCoilBatchTransferHandling,
  auxMatBatchTransferHandling,
  otherBatchTransferHandling,
  gasBatchTransferHandling
} from '@/api/wms/material-transfer/raw-material/transfer-handling'
import { defineEmits, defineProps, ref, watchEffect, computed } from 'vue'
import { matClsEnum } from '@/utils/enum/modules/classification'
import { materialOperateColumns } from '@/utils/columns-format/wms'

import useVisible from '@compos/use-visible'
import useMaxHeight from '@compos/use-max-height'
import ProjectCascader from '@comp-base/project-cascader.vue'
import useProjectTree from '@compos/store/use-project-tree'
import WorkshopSelect from '@/components-system/base/workshop-select.vue'
import WarehouseSelect from '@/components-system/wms/warehouse-select.vue'
import elExpandTableColumn from '@comp-common/el-expand-table-column.vue'
import expandSecondaryInfo from '@/components-system/wms/table-columns/expand-secondary-info/index.vue'
import materialBaseInfoColumns from '@/components-system/wms/table-columns/material-base-info-columns/index.vue'
import materialUnitOperateQuantityColumns from '@/components-system/wms/table-columns/material-unit-operate-quantity-columns/index.vue'
import materialSecondaryInfoColumns from '@/components-system/wms/table-columns/material-secondary-info-columns/index.vue'
import warehouseInfoColumns from '@/components-system/wms/table-columns/warehouse-info-columns/index.vue'
import { transferNormalTypeEnum } from '@/utils/enum/modules/wms'
import { ElMessage } from 'element-plus'
import { numFmtByUnitForList } from '@/utils/wms/convert-unit'

const emit = defineEmits(['success', 'update:visible'])

const props = defineProps({
  visible: {
    type: Boolean,
    require: true
  },
  basicClass: {
    // 基础分类
    type: Number
  },
  materialList: {
    // 物料调拨信息
    type: Array,
    default: () => []
  }
})

const { projectMap, monomerMap } = useProjectTree()

// 校验
const rules = {
  transferType: [{ required: true, message: '请选择调拨类型', trigger: 'change' }],
  projectId: [{ required: true, message: '请选择调拨项目', trigger: 'change' }],
  workshopId: [{ required: true, message: '请选择调拨车间', trigger: 'change' }],
  warehouseId: [{ required: true, message: '请选择调拨仓库', trigger: 'change' }]
}

// 表单ref
const formRef = ref()
// 表格展开key列表
const expandRowKeys = ref([])
// 禁用的调拨类型
const disabledTransferType = ref([])
// 过滤后的材料列表
const materialList = ref([])
// 提交表单
const form = ref({
  list: [],
  transferType: transferNormalTypeEnum.PROJECT_WARE.V
})
// 提交loading
const submitLoading = ref(false)
// 表格列数据格式转换
const columnsDataFormat = ref([...materialOperateColumns])
// 显示
const { visible: dialogVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook: clearValidate })
// 表格最大高度
const { maxHeight } = useMaxHeight(
  {
    mainBox: '.wms-batch-transfer-handling',
    extraBox: ['.el-dialog__header', '.form-header'],
    wrapperBox: ['.el-dialog__body'],
    clientHRepMainH: true,
    navbar: false,
    minHeight: 350
  },
  dialogVisible
)

// 显示项目选择 ：项目仓，且配置为可出库到其他项目的情况下可选择
const showProjectSelect = computed(() => {
  return form.value.transferType === transferNormalTypeEnum.PROJECT_WARE.V
})

// 显示仓库位置选择：归还甲方的情况下无需选择
const showWorkshopAndWare = computed(() => {
  return form.value.transferType !== transferNormalTypeEnum.RETURN_PARTY_A.V
})

// 备注输入框大小
const remarkTextSize = computed(() => {
  if (props.basicClass === matClsEnum.STEEL_PLATE.V) {
    return { minRows: 2, maxRows: 2 }
  }
  return { minRows: 1, maxRows: 1 }
})

// 监听传入的列表
watchEffect(() => {
  // 无需在打开dlg时，判断batchTransferQuantity是否大于corOperableQuantity，因为当corOperableQuantity发生变化时，页面及数据会刷新
  let partyANum = 0
  materialList.value = props.materialList.filter((v) => v.corOperableQuantity > 0) // 过滤不可操作的列表
  form.value.list = materialList.value
  materialList.value.forEach((v) => {
    if (v.boolPartyA) partyANum++
  })
  if (partyANum) {
    disabledTransferType.value = []
  } else {
    disabledTransferType.value = [transferNormalTypeEnum.RETURN_PARTY_A.V]
  }
})

// 表单初始化
function formInit() {
  form.value = {
    list: [],
    transferType: transferNormalTypeEnum.PROJECT_WARE.V
  }
  formRef.value && formRef.value.resetFields()
}

// 关闭回调
function clearValidate() {
  formRef.value && formRef.value.clearValidate()
}

// 设置最大数量
function setMaxQuantity() {
  form.value.list.forEach((v) => {
    v.batchTransferQuantity = v.corOperableQuantity
  })
}

// 清空数量
function clearQuantity() {
  form.value.list.forEach((v) => {
    v.batchTransferQuantity = undefined
  })
}

// 批量调拨提交
async function submit() {
  try {
    submitLoading.value = true
    const valid = await formRef.value.validate()
    if (!valid) return
    const submitApi = getApi(props.basicClass)
    // 数据格式装换
    const data = {
      transferType: form.value.transferType, // 调拨类型
      projectId: form.value.projectId, // 项目id
      monomerId: form.value.monomerId, // 单体id
      areaId: form.value.areaId, // 区域id
      workshopId: form.value.workshopId, // 车间id
      warehouseId: form.value.warehouseId, // 仓库id
      list: []
    }
    // 无需进行对列表进行数量是否填写校验，提交时过滤数量为空或为0的数据
    form.value.list.forEach((v) => {
      if (v.batchTransferQuantity) {
        data.list.push({
          id: v.id,
          quantity: v.batchTransferQuantity, // 数量
          outboundUnit: v.outboundUnit, // 出库单位
          outboundUnitPrecision: v.outboundUnitPrecision, // 单位精度
          outboundUnitType: v.outboundUnitType, // 出库单位类型
          remark: v.remark // 备注
        })
      }
    })
    await numFmtByUnitForList(data.list, {
      unitField: 'outboundUnit',
      unitPrecisionField: 'outboundUnitPrecision',
      fields: ['quantity'],
      toSmallest: true,
      toNum: true
    })
    if (data.list.length === 0) {
      ElMessage.warning('请填写数据')
      return
    }
    await submitApi(data)
    ElMessage.success('已提交调拨申请')
    emit('success')
    handleClose()
    setTimeout(() => {
      formInit()
    }, 0)
  } catch (error) {
    console.log('调拨办理', error)
  } finally {
    submitLoading.value = false
  }
}

// 调拨类型发生变化
function handleTypeChange(type) {
  if (type === transferNormalTypeEnum.RETURN_PARTY_A.V) {
    // 归还甲方，筛选为甲供的类型
    form.value.list = materialList.value.filter((v) => v.boolPartyA)
  } else {
    form.value.list = materialList.value
  }
}

// 批量调拨api
function getApi(basicClass) {
  switch (basicClass) {
    case matClsEnum.STEEL_PLATE.V:
      return steelPlateBatchTransferHandling
    case matClsEnum.SECTION_STEEL.V:
      return sectionSteelBatchTransferHandling
    case matClsEnum.STEEL_COIL.V:
      return steelCoilBatchTransferHandling
    case matClsEnum.MATERIAL.V:
      return auxMatBatchTransferHandling
    case matClsEnum.OTHER.V:
      return otherBatchTransferHandling
    case matClsEnum.GAS.V:
      return gasBatchTransferHandling
    default:
      return null
  }
}
</script>
