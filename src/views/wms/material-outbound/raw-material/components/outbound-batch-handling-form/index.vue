<template>
  <common-dialog
    title="出库办理"
    v-model="dialogVisible"
    width="90%"
    :before-close="handleClose"
    :show-close="true"
    custom-class="wms-batch-outbound-handling"
    top="10vh"
  >
    <template #titleRight>
      <common-button :loading="submitLoading" size="mini" type="primary" @click="submit"> 提 交 </common-button>
    </template>
    <el-form ref="formRef" class="form" :model="form" :rules="rules" size="small" label-position="right" inline label-width="70px">
      <div class="form-header">
        <el-form-item v-if="!showProjectSelect && isBlank(props.projectId)" label="包含项目" label-width="80px">
          <span v-parse-project="{ project: listProjects }" />
        </el-form-item>
        <template v-else>
          <template v-if="showProjectSelect">
            <el-form-item label="项目" prop="projectId" label-width="55px">
              <project-cascader
                v-model="form.projectId"
                clearable
                class="input-underline"
                style="width: 300px"
                @change="handleProjectChange"
              />
            </el-form-item>
            <el-form-item label="单体" prop="monomerId" label-width="55px">
              <common-select
                v-model="form.monomerId"
                :options="(form.projectId && projectMap?.[form.projectId]?.children) || []"
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
                :options="(form.monomerId && monomerMap?.[form.monomerId]?.children) || []"
                :dataStructure="{ key: 'id', label: 'name', value: 'id' }"
                class="input-underline"
                clearable
                type="other"
                placeholder="可选择区域"
                style="width: 200px"
              />
            </el-form-item>
          </template>
          <el-form-item v-else label="项目" prop="projectId" label-width="55px">
            <span v-parse-project="{ project: currentProject }" v-empty-text style="display: inline-block; min-width: 150px" />
          </el-form-item>
        </template>
        <el-form-item label="领用人" prop="recipientId">
          <user-dept-cascader
            v-model="form.recipientId"
            :collapse-tags="false"
            clearable
            filterable
            show-all-levels
            placeholder="领用人"
            class="input-underline"
            style="width: 300px"
          />
        </el-form-item>
      </div>
      <common-table
        ref="tableRef"
        :data="form.list"
        :data-format="columnsDataFormat"
        :max-height="maxHeight"
        :default-expand-all="false"
        :cell-class-name="wrongCellMask"
        :expand-row-keys="expandRowKeys"
        row-key="id"
      >
        <el-expand-table-column :data="form.list" v-model:expand-row-keys="expandRowKeys" row-key="id" fixed="left">
          <template #default="{ row }">
            <expand-secondary-info :basic-class="row.basicClass" :row="row" show-graphics>
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
        <material-unit-operate-quantity-columns
          :operable-quantity-field="boolPublicWare ? 'projectOperableQuantity' : 'operableQuantity'"
          :operable-mete-field="boolPublicWare ? 'projectOperableMete' : 'operableMete'"
          :basic-class="basicClass"
        />
        <!-- 次要信息 -->
        <material-secondary-info-columns :basic-class="basicClass" :show-batch-no="false" />
        <warehouse-info-columns />
        <el-table-column label="车间" width="170px" align="center" fixed="right" prop="workshopId">
          <template #default="{ row: { sourceRow: row } }">
            <workshop-select
              v-model="row.workshopId"
              :factory-id="row.factory?.id"
              placeholder="可选择车间"
              style="width: 100%"
              clearable
            />
          </template>
        </el-table-column>
        <el-table-column label="出库数量" width="170px" align="center" fixed="right">
          <template #header>
            <span>出库数量</span>
            <span class="text-clickable" style="margin-left: 10px" @click="setMaxQuantity">全部出库</span>
            <span class="text-clickable" style="margin-left: 10px" @click="clearQuantity">清空</span>
          </template>
          <template #default="{ row: { sourceRow: row } }">
            <span class="flex-rbc">
              <common-input-number
                v-model="row.batchOutboundQuantity"
                :min="0"
                :precision="row.outboundUnitPrecision"
                :max="row.corProjectOperableQuantity"
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
  steelPlateBatchOutboundHandling,
  sectionSteelBatchOutboundHandling,
  steelCoilBatchOutboundHandling,
  auxMatBatchOutboundHandling,
  gasBatchOutboundHandling,
} from '@/api/wms/material-outbound/raw-material/outbound-handling'
import { defineEmits, defineProps, watch, ref, computed, nextTick } from 'vue'
import { mapGetters } from '@/store/lib'
import { STEEL_ENUM } from '@/settings/config'
import { matClsEnum } from '@/utils/enum/modules/classification'
import { measureTypeEnum, projectWarehouseTypeEnum } from '@/utils/enum/modules/wms'
import { obj2arr } from '@/utils/convert/type'
import { isBlank } from '@/utils/data-type'
import { numFmtByUnitForList } from '@/utils/wms/convert-unit'
import { materialOperateColumns } from '@/utils/columns-format/wms'
import { getProjectInfo } from '@/utils/project'

import useVisible from '@compos/use-visible'
import useTableValidate from '@compos/form/use-table-validate'
import useMaxHeight from '@compos/use-max-height'
import useWmsConfig from '@/composables/store/use-wms-config'
import userDeptCascader from '@comp-base/user-dept-cascader.vue'
import useProjectTree from '@compos/store/use-project-tree'
import projectCascader from '@comp-base/project-cascader.vue'
import elExpandTableColumn from '@comp-common/el-expand-table-column.vue'
import expandSecondaryInfo from '@/components-system/wms/table-columns/expand-secondary-info/index.vue'
import materialBaseInfoColumns from '@/components-system/wms/table-columns/material-base-info-columns/index.vue'
import materialUnitOperateQuantityColumns from '@/components-system/wms/table-columns/material-unit-operate-quantity-columns/index.vue'
import materialSecondaryInfoColumns from '@/components-system/wms/table-columns/material-secondary-info-columns/index.vue'
import warehouseInfoColumns from '@/components-system/wms/table-columns/warehouse-info-columns/index.vue'
import workshopSelect from '@/components-system/base/workshop-select.vue'
import { ElMessage } from 'element-plus'

const emit = defineEmits(['success', 'update:visible'])
const props = defineProps({
  visible: {
    type: Boolean,
    require: true,
  },
  projectWarehouseType: {
    type: Number,
  },
  projectId: {
    type: Number,
  },
  basicClass: {
    // 基础分类
    type: Number,
  },
  materialList: {
    // 物料出库信息
    type: Array,
    default: () => [],
  },
})

const { projectMap, monomerMap } = useProjectTree()

// 钢板校验规则
const steelRules = {
  projectId: [{ required: true, message: '请选择出库项目', trigger: 'change' }],
}
const tableRules = {
  workshopId: [{ required: true, message: '请选择出库车间', trigger: 'change' }],
}
const { tableValidate, wrongCellMask } = useTableValidate({ rules: tableRules, errorMsg: '请选择出库车间' }) // 表格校验

// 校验
const rules = computed(() => {
  if (props.basicClass & STEEL_ENUM) {
    return steelRules
  }
  return {}
})

// 表单ref
const formRef = ref()
// 表格展开key列表
const expandRowKeys = ref([])
// 过滤后的材料列表
const materialList = ref([])
// 当前项目
const currentProject = ref()
// 提交表单
const form = ref({
  list: [],
  recipientId: undefined, // 领用人id
})
// 提交loading
const submitLoading = ref(false)
// 表格列数据格式转换
const columnsDataFormat = ref([
  ...materialOperateColumns,
  ['projectOperableQuantity', ['to-fixed-field', 'measurePrecision']],
  ['projectOperableMete', ['to-fixed-field', 'accountingPrecision']],
])
// 显示
const { visible: dialogVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook: clearValidate })
// 表格最大高度
const { maxHeight } = useMaxHeight(
  {
    mainBox: '.wms-batch-outbound-handling',
    extraBox: ['.el-dialog__header', '.form-header'],
    wrapperBox: ['.el-dialog__body'],
    clientHRepMainH: true,
    navbar: false,
    minHeight: 350,
  },
  dialogVisible
)
// 出库配置
const { outboundCfg } = useWmsConfig()

// 当前用户
const { user } = mapGetters('user')

const boolPublicWare = computed(() => props.projectWarehouseType === projectWarehouseTypeEnum.PUBLIC.V)

// 显示项目选择组件(false:显示项目名称)： 公共库 或者 配置=>项目库可以出库给其他项目
const showProjectSelect = computed(() => {
  return boolPublicWare.value || outboundCfg.value.boolCanOutToOtherProject === true
})

const listProjects = computed(() => {
  const projects = {}
  form.value.list.forEach((l) => {
    if (!projects[l.project.id]) {
      projects[l.project.id] = l.project
    }
  })
  return obj2arr(projects)
})

// 备注输入框大小
const remarkTextSize = computed(() => {
  if (props.basicClass === matClsEnum.STEEL_PLATE.V) {
    return { minRows: 2, maxRows: 2 }
  }
  return { minRows: 1, maxRows: 1 }
})

// 监听项目及仓库类型变化
watch(
  [() => props.projectId, () => props.projectWarehouseType],
  ([pId, type], [oldPId]) => {
    if (type === projectWarehouseTypeEnum.PUBLIC.V) {
      form.value.projectId = undefined
    } else {
      form.value.projectId = pId
    }
    if (oldPId !== pId) {
      currentProject.value = getProjectInfo(pId)
    }
  },
  { immediate: true }
)

const setRecipientId = watch(
  dialogVisible,
  (visible) => {
    if (visible) {
      form.value.recipientId = user.value.id // 领用人id
      nextTick(() => {
        // 首次设置默认领用人
        setRecipientId()
      })
    }
  },
  {
    immediate: true,
  }
)

// 监听传入的列表
watch(
  () => props.materialList,
  () => {
    // 无需在打开dlg时，判断batchOutboundQuantity是否大于corOperableQuantity，因为当corOperableQuantity发生变化时，页面及数据会刷新
    materialList.value = props.materialList.filter((v) => v.corOperableQuantity > 0) // 过滤不可操作的列表
    form.value.list = materialList.value
    dataFormat()
  }
)

// 表单初始化
function formInit() {
  form.value = { list: [] }
  formRef.value && formRef.value.resetFields()
  form.value.recipientId = user.value.id // 领用人id
}

// 关闭回调
function clearValidate() {
  formRef.value && formRef.value.clearValidate()
}

// 设置最大数量
function setMaxQuantity() {
  form.value.list.forEach((v) => {
    v.batchOutboundQuantity = boolPublicWare.value ? v.corProjectOperableQuantity : v.corOperableQuantity
  })
}

// 清空数量
function clearQuantity() {
  form.value.list.forEach((v) => {
    v.batchOutboundQuantity = undefined
  })
}

// 批量出库提交
async function submit() {
  try {
    submitLoading.value = true
    const valid = await formRef.value.validate()
    if (!valid) return
    const submitApi = getApi(props.basicClass)
    // 数据格式装换
    const data = {
      projectId: form.value.projectId,
      monomerId: form.value.monomerId,
      areaId: form.value.areaId,
      recipientId: form.value.recipientId,
      list: [],
    }
    // 无需进行对列表进行数量是否填写校验，提交时过滤数量为空或为0的数据
    form.value.list.forEach((v) => {
      if (v.batchOutboundQuantity) {
        data.list.push({
          id: v.id,
          workshopId: v.workshopId, // 车间id
          quantity: v.batchOutboundQuantity, // 数量
          outboundUnit: v.outboundUnit, // 出库单位
          outboundUnitPrecision: v.outboundUnitPrecision, // 单位精度
          outboundUnitType: v.outboundUnitType, // 出库单位类型
          remark: v.remark, // 备注
        })
      }
    })
    await numFmtByUnitForList(data.list, {
      unitField: 'outboundUnit',
      unitPrecisionField: 'outboundUnitPrecision',
      fields: ['quantity'],
      toSmallest: true,
      toNum: true,
    })
    const { validResult, dealList } = tableValidate(data.list)
    if (validResult) {
      data.list = dealList
    } else {
      return validResult
    }
    if (data.list.length === 0) {
      ElMessage.warning('请填写数据')
      return
    }
    await submitApi(data)
    ElMessage.success('已加入出库清单')
    emit('success')
    handleClose()
    setTimeout(() => {
      formInit()
    }, 0)
  } catch (error) {
    console.log('出库办理', error)
  } finally {
    submitLoading.value = false
  }
}

// 项目发生变化
function handleProjectChange(val) {
  if (val) {
    form.value.list = materialList.value.filter((v) => {
      // 甲供无法跨项目出库，因此过滤不是当前项目的甲供材料
      const flag = v.boolPartyA !== true || (v.boolPartyA === true && v.project.id === val)
      return flag
    })
  } else {
    form.value.list = materialList.value
  }
  dataFormat()
}

// 数据格式化
function dataFormat() {
  if (props.projectWarehouseType === projectWarehouseTypeEnum.PUBLIC.V) {
    // 公共库的情况，重新计算最大数量
    form.value.list.forEach((v) => {
      // 车间id
      v.factoryId = v.factory?.id
      // 最大数量换算
      const hasProjectFrozen = !form.value.projectId || !v.projectFrozenKV || !v.projectFrozenKV[form.value.projectId]
      if (hasProjectFrozen) {
        v.projectOperableQuantity = v.operableQuantity
        v.projectOperableMete = v.operableMete
        v.corProjectOperableQuantity = v.corOperableQuantity
      } else {
        const projectFrozen = v.projectFrozenKV[form.value.projectId]
        v.projectOperableQuantity = v.operableQuantity + (projectFrozen.quantity || 0)
        v.projectOperableMete = v.operableMete + (projectFrozen.mete || 0)
      }
      v.corProjectOperableQuantity = v.outboundUnitType === measureTypeEnum.MEASURE.V ? v.projectOperableQuantity : v.projectOperableMete
    })
  } else {
    // 项目库的情况
    form.value.list.forEach((v) => {
      // 车间id
      v.factoryId = v.factory?.id
      v.corProjectOperableQuantity = v.outboundUnitType === measureTypeEnum.MEASURE.V ? v.operableQuantity : v.operableMete
    })
  }
}

// 批量出库api
function getApi(basicClass) {
  switch (basicClass) {
    case matClsEnum.STEEL_PLATE.V:
      return steelPlateBatchOutboundHandling
    case matClsEnum.SECTION_STEEL.V:
      return sectionSteelBatchOutboundHandling
    case matClsEnum.STEEL_COIL.V:
      return steelCoilBatchOutboundHandling
    case matClsEnum.MATERIAL.V:
      return auxMatBatchOutboundHandling
    case matClsEnum.GAS.V:
      return gasBatchOutboundHandling
    default:
      return null
  }
}
</script>
