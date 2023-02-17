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
          <el-radio :label="true" :disabled="props.padBlockData?.length && !props.checkedNodes?.length">使用新工单</el-radio>
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
      </div>
    </template>
    <template #titleRight>
      <common-button @click="submitIt" :loading="submitLoading" size="mini" type="primary">保存</common-button>
    </template>
    <div class="head-container">
      <el-form style="display: flex; flex-wrap: wrap" v-if="isNew" :rules="rules">
        <el-form-item label="材质:" class="form-label-require">
          <common-select
            v-model="material"
            :options="materialList"
            :dataStructure="{ key: 'name', label: 'name', value: 'name' }"
            clearable
            filterable
            allow-create
            type="other"
            class="filter-item"
            placeholder="请选择材质"
            style="width: 160px"
          />
        </el-form-item>
        <el-form-item label="厚度:" class="form-label-require">
          <common-select
            v-model="thick"
            :options="thickDataOption"
            :dataStructure="{ key: 'name', label: 'name', value: 'name' }"
            clearable
            filterable
            allow-create
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
        <el-form-item label="排产日期:" class="form-label-require">
          <el-date-picker
            v-model="askCompleteTime"
            type="date"
            size="small"
            class="date-item filter-item"
            style="width: 160px !important"
            placeholder="选择排产日期"
            :clearable="false"
            format="YYYY-MM-DD"
            value-format="x"
            :disabled-date="disabledDate"
          />
        </el-form-item>
      </el-form>
    </div>
    <common-table :data="list" :data-format="dataFormat" :max-height="maxHeight - 100" style="width: 100%">
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column :show-overflow-tooltip="true" prop="project" label="所属项目" min-width="120px" align="center">
        <template #default="{ row }">
          <table-cell-tag v-if="!row.needMachinePartLinkList" color="#1890ff" name="标准零件" />
          <span v-if="row.project && row.needMachinePartLinkList">{{ row.project }}</span>
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
      <el-table-column prop="quantity" :show-overflow-tooltip="true" label="数量" width="80" align="center">
        <template #default="{ row }">
          <span v-if="!row.needMachinePartLinkList">{{ row.usedQuantity }}</span>
          <span v-else>{{ row.quantity }}</span>
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
import { newSave, getCutTaskDetail, getHoleTaskDetail } from '@/api/mes/scheduling-manage/machine-part'
import { ElMessage, ElNotification, ElRadioGroup, ElMessageBox } from 'element-plus'
import { defineEmits, defineProps, ref, computed } from 'vue'
import { layOffWayTypeEnum } from '@enum-ms/uploading-form'
// import { materialTypeEnum } from '@enum-ms/uploading-form'
import { componentTypeEnum, machinePartIssuedWayEnum, machinePartSchedulingIssueStatusEnum as issueStatusEnum } from '@enum-ms/mes'
import { manualFetchGroupsTree } from '@compos/mes/scheduling/use-scheduling-groups'
import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
import cutConfigSelect from '@/components-system/base/cut-config-select.vue'
import drillSchedulingDialog from './drill-scheduling-dialog.vue'

const emit = defineEmits(['update:visible', 'success'])
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
  },
  queryThickList: {
    type: Array,
    default: () => []
  }
})
const dataFormat = ref([['project', 'parse-project']])
const totalList = ref([])
const drillData = ref({})
const thickDataOption = computed(() => {
  const arr = []
  for (let i = 0; i < props.list.length; i++) {
    if (arr.findIndex((k) => k.name === props.list[i].thick) < 0) {
      arr.push({ name: props.list[i].thick })
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
    material: material.value,
    thick: thick.value,
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

function showHook() {
  groupsId.value = undefined
  material.value = undefined
  thick.value = undefined
  askCompleteTime.value = undefined
  cutConfigId.value = undefined
  fetchGroups()
  if (props.type === 1) {
    isNew.value = true
    saveType.value = machinePartIssuedWayEnum.NESTING_ISSUED.V
    if (props.padBlockData?.length && !props.checkedNodes?.length) {
      isNew.value = false
      saveType.value = machinePartIssuedWayEnum.ADD_NEW_TICKET.V
    }
  } else {
    isNew.value = true
    saveType.value = machinePartIssuedWayEnum.NESTING_ISSUED.V
  }
  fetchOrder()
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
    if (!thick.value || !material.value || !groupsId.value || !askCompleteTime.value || !cutConfigId.value) {
      ElMessage.warning('必选项不能为空')
      return false
    }
  }
  if (props.type === 1 && isNew.value) {
    if (!thick.value || !material.value || !askCompleteTime.value || !cutConfigId.value) {
      ElMessage.warning('必选项不能为空')
      return false
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
      if (v.needMachinePartLinkList) {
        v.needMachinePartLinkList?.forEach((o) => {
          _list.push({
            productId: v.id,
            quantity: o.quantity,
            id: o.id,
            needSchedulingMonth: o.date
          })
        })
      } else {
        _list.push({
          productId: v.id,
          quantity: v.quantity
        })
      }
    })
    totalList.value.forEach((v) => {
      _partIds.push({
        id: v.id,
        quantity: v.quantity
      })
    })
    console.log(totalList.value, schedulingId.value, 'totalList.value')
    if (props.queryThickList.length > 1) {
      await ElMessageBox.confirm('您将不同板厚或材质的零件进行了套料，是否确定？', '提示', {
        confirmButtonText: '确定',
        cancelButtonText: '取消',
        type: 'warning'
      })
    }
    if (props.type === 0) {
      const data = await getHoleTaskDetail({
        thick: thick.value,
        cutConfigId: cutConfigId.value,
        partList: _partIds
      })
      if (data?.boolDrillEnum) {
        drillDialogVisible.value = true
        drillData.value = data || {}
      } else {
        await newSave({
          groupsId: groupsId.value,
          material: material.value,
          thick: thick.value,
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
      await newSave({
        groupsId: groupsId.value,
        material: material.value,
        thick: thick.value,
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
    handleClose()
    emit('success')
  } catch (error) {
    console.log('保存零件排产报错', error)
  } finally {
    submitLoading.value = false
  }
}
</script>
