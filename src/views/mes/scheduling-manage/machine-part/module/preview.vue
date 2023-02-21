<template>
  <common-dialog
    customClass="machine-part-scheduling-preview-dlg"
    title="零件排产预览"
    v-model="dialogVisible"
    width="1500px"
    top="5vh"
    :before-close="handleClose"
  >
    <template #titleAfter>
      <div style="display: flex">
        <el-radio-group v-if="type === 1" v-model="isNew">
          <el-radio :label="true" :disabled="Boolean(props.padBlockData?.length) && !props.checkedNodes?.length">使用新工单</el-radio>
          <el-radio :label="false">使用原有工单</el-radio>
        </el-radio-group>
        <div style="margin-left: 15px" v-if="!isNew && type">
          <common-select
            v-model="schedulingId"
            :options="orderList"
            :dataStructure="{ key: 'id', label: 'value', value: 'id' }"
            clearable
            type="other"
            class="filter-item"
            placeholder="请选择原有工单"
            style="width: 240px"
          />
        </div>
        <div style="margin-left: 15px">
          <el-radio-group v-if="type === 1 && isNew" v-model="underLine">
            <el-radio :label="0">正常套料</el-radio>
            <el-radio :label="1">线下套料</el-radio>
          </el-radio-group>
        </div>
      </div>
    </template>
    <template #titleRight>
      <common-button
        @click="submitIt"
        :loading="submitLoading"
        :disabled="
          (!type && Boolean(props.padBlockData?.length) && Boolean(!props.checkedNodes?.length)) ||
          (!props.padBlockData?.length && !props.checkedNodes?.length) ||
          (Boolean(type) &&
            underLine === nestingTypeEnum.OFFLINE.V &&
            Boolean(props.padBlockData?.length) &&
            Boolean(props.checkedNodes?.length))
        "
        size="mini"
        type="primary"
        >保存</common-button
      >
    </template>
    <div class="head-container">
      <el-form style="display: flex; flex-wrap: wrap" v-if="isNew" :rules="rules">
        <el-form-item label="材质:" class="form-label-require" v-show="materialDataOption.length > 1">
          <common-select
            v-model="material"
            :options="materialDataOption"
            :dataStructure="{ key: 'name', label: 'name', value: 'name' }"
            clearable
            filterable
            type="other"
            class="filter-item"
            placeholder="请选择材质"
            style="width: 160px"
          />
        </el-form-item>
        <el-form-item label="厚度:" class="form-label-require" v-show="thickDataOption.length > 1">
          <common-select
            v-model="thick"
            :options="thickDataOption"
            :dataStructure="{ key: 'name', label: 'name', value: 'name' }"
            clearable
            filterable
            type="other"
            class="filter-item"
            placeholder="请选择厚度"
            style="width: 160px"
          />
          <!-- <el-select
          v-model="thick"
          filterable
          allow-create
          :reserve-keyword="false"
          placeholder="选择厚度"
        >
          <el-option v-for="item in thickList" :key="item.value" :label="item.value" :value="item.value" />
        </el-select> -->
        </el-form-item>
        <el-form-item v-if="type && underLine" label="车间:" class="form-label-require">
          <workshop-select
            v-model="workShopId"
            placeholder="请先选择车间"
            style="width: 160px"
            class="filter-item"
            :factory-id="factoryId"
          />
          <!-- <el-cascader
            v-model="workShopId"
            :options="subList"
            check-strictly
            :show-all-levels="false"
            :props="cascaderProps"
            separator=" > "
            clearable
            size="small"
            class="filter-item"
            style="width: 200px"
            placeholder="选择生产线"
            @change="handleProductionLine"
          /> -->
        </el-form-item>
        <el-form-item v-if="!type" label="生产组:" class="form-label-require">
          <el-cascader
            v-model="groupsId"
            :options="schedulingGroups.list"
            :props="{ value: 'id', label: 'name', children: 'children', expandTrigger: 'hover', emitPath: false }"
            :show-all-levels="false"
            style="width: 160px"
            filterable
            clearable
            placeholder="请选择生产组"
          />
        </el-form-item>
        <el-form-item label="切割方式:" class="form-label-require">
          <cut-config-select v-model="cutConfigId" style="width: 160px" :layOffWayType="Boolean(type) ? true : false" clearable />
        </el-form-item>
        <el-form-item label="完成日期:" class="form-label-require">
          <el-date-picker
            v-model="askCompleteTime"
            type="date"
            size="small"
            class="date-item filter-item"
            style="width: 160px !important"
            placeholder="选择完成日期"
            :clearable="false"
            format="YYYY-MM-DD"
            value-format="x"
            :disabled-date="disabledDate"
          />
        </el-form-item>
      </el-form>
    </div>
    <common-table :data="list" return-source-data :max-height="maxHeight - 100" style="width: 100%">
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column :show-overflow-tooltip="true" prop="project" label="所属项目" min-width="120px" align="center">
        <template #default="{ row }">
          <table-cell-tag v-if="!row.needMachinePartLinkList" color="#1890ff" name="标准零件" />
          <span v-if="row.project && row.needMachinePartLinkList">{{ projectNameFormatter(row.project) }}</span>
          <span v-if="!row.needMachinePartLinkList">/</span>
        </template>
      </el-table-column>
      <el-table-column :show-overflow-tooltip="true" prop="serialNumber" label="编号" min-width="80px" align="center">
        <template #default="{ row }">
          <span>{{ row.serialNumber }}</span>
        </template>
      </el-table-column>
      <el-table-column :show-overflow-tooltip="true" prop="specification" label="规格" min-width="80px" align="center">
        <template #default="{ row }">
          <span>{{ row.specification }}</span>
        </template>
      </el-table-column>
      <el-table-column :show-overflow-tooltip="true" prop="thick" label="厚度" min-width="80px" align="center">
        <template #default="{ row }">
          <span style="color: red">{{ row.thick }}</span>
        </template>
      </el-table-column>
      <el-table-column :show-overflow-tooltip="true" prop="specification" label="材质" min-width="80px" align="center">
        <template #default="{ row }">
          <span style="color: red">{{ row.material }}</span>
        </template>
      </el-table-column>
      <el-table-column :show-overflow-tooltip="true" prop="length" :label="`长度(mm)`" min-width="80px" align="center">
        <template #default="{ row }">
          <span>{{ row.length }}</span>
        </template>
      </el-table-column>
      <el-table-column :show-overflow-tooltip="true" prop="netWeight" :label="`单净重(kg)`" min-width="80px" align="center">
        <template #default="{ row }">
          <span>{{ row.netWeight }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="usedQuantity" :show-overflow-tooltip="true" label="数量" min-width="80px" align="center">
        <template #default="{ row }">
          <el-input-number
            v-if="!row.needMachinePartLinkList"
            v-model="row.usedQuantity"
            :step="1"
            :min="1"
            :max="99999999"
            size="mini"
            style="width: 100%"
            controls-position="right"
          />
          <span v-else>{{ row.quantity }}</span>
        </template>
      </el-table-column>
      <el-table-column :show-overflow-tooltip="true" label="操作" width="80" align="center">
        <template #default="{ row }">
          <common-button type="danger" icon="el-icon-delete" size="mini" @click.stop="toDelete(row, row.id)"></common-button>
        </template>
      </el-table-column>
    </common-table>
  </common-dialog>
  <!-- 钻孔排产弹窗 -->
  <drill-scheduling-dialog
    v-model:visible="drillDialogVisible"
    :queryParams="queryParams"
    :total-list="totalList"
    :drill-data="drillData"
    @success="success"
  />
</template>

<script setup>
import { newSave, getCutTaskDetail, getHoleTaskDetail, getOffLineZip } from '@/api/mes/scheduling-manage/machine-part'
// import { getAllFactoryWorkshopLines } from '@/api/mes/common'
import { ElMessage, ElNotification, ElRadioGroup, ElMessageBox } from 'element-plus'
import { defineEmits, defineProps, ref, computed, onMounted, watch } from 'vue'
import { layOffWayTypeEnum } from '@enum-ms/uploading-form'
import { projectNameFormatter } from '@/utils/project'
import { fileDownload } from '@/utils/file'
// import { materialTypeEnum } from '@enum-ms/uploading-form'
import {
  componentTypeEnum,
  machinePartIssuedWayEnum,
  machinePartSchedulingIssueStatusEnum as issueStatusEnum,
  nestingTypeEnum
} from '@enum-ms/mes'
import { manualFetchGroupsTree } from '@compos/mes/scheduling/use-scheduling-groups'
import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
import cutConfigSelect from '@/components-system/base/cut-config-select.vue'
import drillSchedulingDialog from './drill-scheduling-dialog.vue'
import workshopSelect from '@comp-mes/workshop-select'

const emit = defineEmits(['update:visible', 'success', 'handleDel'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  list: {
    type: Array,
    default: () => []
  },
  materialList: {
    type: Array,
    default: () => []
  },
  thickList: {
    type: Array,
    default: () => []
  },
  // materialData: {
  //   type: Array,
  //   default: () => []
  // },
  thickData: {
    type: Array,
    default: () => []
  },
  type: {
    type: Number
  },
  padBlockData: {
    type: Array,
    default: () => []
  },
  checkedNodes: {
    type: Array,
    default: () => []
  }
})
const totalList = ref([])
const drillData = ref({})
const thickDataOption = computed(() => {
  const arr = []
  for (let i = 0; i < props.list.length; i++) {
    if (props.list[i]?.needMachinePartLinkList) {
      if (arr.findIndex((k) => k.name === props.list[i].thick) < 0) {
        arr.push({ name: props.list[i].thick })
      }
    }
  }
  return arr
})

const materialDataOption = computed(() => {
  const arr = []
  for (let i = 0; i < props.list.length; i++) {
    if (props.list[i]?.needMachinePartLinkList) {
      if (arr.findIndex((k) => k.name === props.list[i].material) < 0) {
        arr.push({ name: props.list[i].material })
      }
    }
  }
  return arr
})

// const thickData = ref([])
// const materialData = ref([])

// const layWayConfigId = ref()
// const layingWayList = ref([])
// const layingWayLoading = ref(false)
const submitLoading = ref(false)
const askCompleteTime = ref()
const cutConfigId = ref()
const groupsId = ref()
const thick = ref()
const material = ref()
const isNew = ref(true)
const schedulingId = ref()
const saveType = ref(machinePartIssuedWayEnum.NESTING_ISSUED.V)
const drillDialogVisible = ref(false)
const orderList = ref([])
const underLine = ref(0)
const workShopId = ref()
// const workShopId = ref()
const factoryId = ref()

const rules = {
  material: [{ required: true, message: '请选择材质', trigger: 'blur' }],
  thick: [{ required: true, message: '请选择厚度', trigger: 'blur' }],
  groupsId: [{ required: true, message: '请选择生产班组', trigger: 'blur' }],
  cutConfigId: [{ required: true, message: '请选择切割方式', trigger: 'blur' }],
  askCompleteTime: [{ required: true, message: '请选择排产日期', trigger: 'blur' }]
}

const queryParams = computed(() => {
  return {
    groupsId: groupsId.value,
    material: materialDataOption.value.length > 1 ? material.value : materialDataOption.value[0]?.name,
    thick: thickDataOption.value.length > 1 ? thick.value : thickDataOption.value[0]?.name,
    askCompleteTime: askCompleteTime.value,
    cutConfigId: cutConfigId.value,
    saveType:
      props.type === 0
        ? machinePartIssuedWayEnum.UN_NESTING_ISSUED.V
        : isNew.value === true
          ? machinePartIssuedWayEnum.NESTING_ISSUED.V
          : machinePartIssuedWayEnum.ADD_NEW_TICKET.V
  }
})
const { visible: dialogVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook: showHook })

const { maxHeight } = useMaxHeight(
  {
    mainBox: '.machine-part-scheduling-preview-dlg',
    extraBox: ['.el-dialog__header', 'head-container'],
    wrapperBox: ['.el-dialog__body'],
    clientHRepMainH: true,
    minHeight: 300,
    navbar: false
  },
  dialogVisible
)

function toDelete(row, id) {
  emit('handleDel', row, id)
  if (props.list.findIndex((v) => v.needMachinePartLinkList?.length > 0) < 0) {
    isNew.value = false
    saveType.value = machinePartIssuedWayEnum.ADD_NEW_TICKET.V
  }
  // emit('success')
}

function showHook() {
  groupsId.value = undefined
  material.value = undefined
  thick.value = undefined
  askCompleteTime.value = undefined
  cutConfigId.value = undefined
  workShopId.value = undefined
  fetchGroups()
  fetchOrder()
  isNew.value = true
  saveType.value = machinePartIssuedWayEnum.NESTING_ISSUED.V
  if (props.type === 1) {
    if (Boolean(props.padBlockData?.length) && !props.checkedNodes?.length) {
      isNew.value = false
      saveType.value = machinePartIssuedWayEnum.ADD_NEW_TICKET.V
    }
  }
  // success()
}

function success() {
  emit('success')
}
async function fetchOrder() {
  try {
    const { content } = await getCutTaskDetail({
      boolNestCutEnum: layOffWayTypeEnum.NESTING.V,
      issueStatusEnum: issueStatusEnum.IN_NESTING.V
    })
    content?.forEach((v) => {
      orderList.value.push({
        id: v.id,
        value: v.orderNumber
      })
    })
  } catch (error) {
    console.log('获取工单失败', error)
  }
}
// --------------------------- 获取生产线 start ------------------------------

// const subList = ref([])

// const cascaderProps = computed(() => {
//   return {
//     value: 'id',
//     label: 'name',
//     children: 'children',
//     checkStrictly: props.checkStrictly,
//     expandTrigger: props.expandTrigger,
//     emitPath: props.emitPath,
//     multiple: props.multiple
//   }
// })
// watch(
//   () => props.modelValue,
//   (value) => {
//     if (value instanceof Array) {
//       workShopId.value = [...value]
//     } else {
//       workShopId.value = value
//     }
//     handleChange(value)
//   },
//   { immediate: true }
// )
// onMounted(() => allFactoryWorkshopLines())

// async function allFactoryWorkshopLines() {
//   try {
//     const { content } = await getAllFactoryWorkshopLines({})
//     content.forEach((v) => {
//       v.children = v.workshopList
//       v.workshopList.forEach((p) => {
//         p.children = p.productionLineList
//       })
//     })
//     subList.value = content
//   } catch (error) {
//     console.log('请求工厂-车间-生产线的层级接口失败')
//   }
// }
// function handleProductionLine(val) {
//   console.log(val, 'val')
// }
// function handleChange(val) {
//   emit('update:modelValue', val)
//   emit('change', val)
// }
// --------------------------- 获取生产线 end ------------------------------

// --------------------------- 获取生产班组 start ------------------------------
const groupLoad = ref(false)
const schedulingGroups = ref({ list: [], obj: {}})

async function fetchGroups() {
  if (groupLoad.value) return
  try {
    schedulingGroups.value = await manualFetchGroupsTree({ productType: componentTypeEnum.MACHINE_PART.V })
    groupLoad.value = true
  } catch (e) {
    console.log('获取生产组的信息失败', e)
  }
}
// --------------------------- 获取生产班组 end --------------------------------
function disabledDate(time) {
  return time < new Date()
}
async function submitIt() {
  if (props.type === 0) {
    if (thickDataOption.value.length > 1 && materialDataOption.value.length > 1) {
      if (!thick.value || !material.value || !groupsId.value || !askCompleteTime.value || !cutConfigId.value) {
        ElMessage.warning('必选项不能为空')
        return false
      }
    }
    if (thickDataOption.value.length === 1 && materialDataOption.value.length > 1) {
      if (!material.value || !groupsId.value || !askCompleteTime.value || !cutConfigId.value) {
        ElMessage.warning('必选项不能为空')
        return false
      }
    }
    if (thickDataOption.value.length > 1 && materialDataOption.value.length === 1) {
      if (!thick.value || !groupsId.value || !askCompleteTime.value || !cutConfigId.value) {
        ElMessage.warning('必选项不能为空')
        return false
      }
    }
    if (thickDataOption.value.length === 1 && materialDataOption.value.length === 1) {
      if (!groupsId.value || !askCompleteTime.value || !cutConfigId.value) {
        ElMessage.warning('必选项不能为空')
        return false
      }
    }
  }
  if (props.type === 1 && isNew.value) {
    if (thickDataOption.value.length > 1 && materialDataOption.value.length > 1 && underLine.value) {
      if (!workShopId.value || !thick.value || !material.value || !askCompleteTime.value || !cutConfigId.value) {
        ElMessage.warning('必选项不能为空')
        return false
      }
    }
    if (thickDataOption.value.length === 1 && materialDataOption.value.length > 1 && underLine.value) {
      if (!workShopId.value || !material.value || !askCompleteTime.value || !cutConfigId.value) {
        ElMessage.warning('必选项不能为空')
        return false
      }
    }
    if (thickDataOption.value.length > 1 && materialDataOption.value.length === 1 && underLine.value) {
      if (!workShopId.value || !thick.value || !askCompleteTime.value || !cutConfigId.value) {
        ElMessage.warning('必选项不能为空')
        return false
      }
    }
    if (thickDataOption.value.length === 1 && materialDataOption.value.length === 1 && underLine.value) {
      if (!workShopId.value || !askCompleteTime.value || !cutConfigId.value) {
        ElMessage.warning('必选项不能为空')
        return false
      }
    }
  }
  if (props.type === 1 && !isNew.value) {
    if (!schedulingId.value) {
      ElMessage.warning('必选项不能为空')
      return false
    }
  }

  try {
    submitLoading.value = true
    const _list = []
    const _partIds = []
    totalList.value = []
    props.list.forEach((v) => {
      totalList.value.push(v)
      if (v.needMachinePartLinkList?.length > 0) {
        console.log(v, 'v')
        v.needMachinePartLinkList?.forEach((o) => {
          _list.push({
            productId: v.id,
            quantity: o.quantity,
            id: o.id,
            needSchedulingMonth: o.date
          })
        })
      }
      if (!v.needMachinePartLinkList) {
        _list.push({
          productId: v.id,
          quantity: v.usedQuantity
        })
      }
    })
    totalList.value.forEach((v) => {
      _partIds.push({
        id: v.id,
        quantity: !v.needMachinePartLinkList ? v.usedQuantity : v.quantity
      })
    })
    console.log(totalList.value, schedulingId.value, 'totalList.value')
    if (materialDataOption.value.length > 1 || thickDataOption.value.length > 1) {
      await ElMessageBox.confirm('您将不同板厚或材质的零件进行了套料，是否确定？', '提示', {
        confirmButtonText: '确定',
        cancelButtonText: '取消',
        type: 'warning'
      })
    }
    if (props.type === 0) {
      const data = await getHoleTaskDetail({
        thick: thickDataOption.value.length > 1 ? thick.value : thickDataOption.value[0]?.name,
        cutConfigId: cutConfigId.value,
        partList: _partIds
      })
      if (data?.boolDrillEnum) {
        drillDialogVisible.value = true
        drillData.value = data || {}
      } else {
        await newSave({
          groupsId: groupsId.value,
          material: materialDataOption.value.length > 1 ? material.value : materialDataOption.value[0]?.name,
          thick: thickDataOption.value.length > 1 ? thick.value : thickDataOption.value[0]?.name,
          boolOffLine: nestingTypeEnum.NORMAL.V,
          schedulingId: schedulingId.value,
          linkList: _list,
          askCompleteTime: askCompleteTime.value,
          cutConfigId: cutConfigId.value,
          saveType:
            props.type === 0
              ? machinePartIssuedWayEnum.UN_NESTING_ISSUED.V
              : isNew.value === true
                ? machinePartIssuedWayEnum.NESTING_ISSUED.V
                : machinePartIssuedWayEnum.ADD_NEW_TICKET.V
        })
        ElNotification({
          title: '零件排产保存成功',
          type: 'success',
          duration: 2500
        })
      }
    } else {
      const _data = await newSave({
        groupsId: groupsId.value,
        material: materialDataOption.value.length > 1 ? material.value : materialDataOption.value[0]?.name,
        thick: thickDataOption.value.length > 1 ? thick.value : thickDataOption.value[0]?.name,
        boolOffLine:
          isNew.value === false
            ? nestingTypeEnum.NORMAL.V
            : underLine.value === nestingTypeEnum.OFFLINE.V
              ? nestingTypeEnum.OFFLINE.V
              : nestingTypeEnum.NORMAL.V,
        workShopId: underLine.value ? workShopId.value : undefined,
        schedulingId: schedulingId.value,
        linkList: _list,
        askCompleteTime: askCompleteTime.value,
        cutConfigId: cutConfigId.value,
        saveType:
          props.type === 0
            ? machinePartIssuedWayEnum.UN_NESTING_ISSUED.V
            : isNew.value === true
              ? machinePartIssuedWayEnum.NESTING_ISSUED.V
              : machinePartIssuedWayEnum.ADD_NEW_TICKET.V
      })
      if (underLine.value === nestingTypeEnum.OFFLINE.V) {
        await fileDownload(getOffLineZip, { id: _data })
      }
      ElNotification({
        title: '零件排产保存成功',
        type: 'success',
        duration: 2500
      })
    }
    handleClose()
    emit('success')
  } catch (error) {
    console.log('保存零件排产报错', error)
  } finally {
    submitLoading.value = false
  }
}
</script>
