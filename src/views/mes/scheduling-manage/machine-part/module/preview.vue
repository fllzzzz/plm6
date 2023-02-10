<template>
  <common-dialog
    customClass="machine-part-scheduling-preview-dlg"
    title="零件排产预览"
    v-model="dialogVisible"
    width="1500px"
    :before-close="handleClose"
  >
    <template #titleAfter>
      <el-radio-group v-if="type === 1" v-model="isNew">
        <el-radio :label="true">使用新工单</el-radio>
        <el-radio :label="false">使用原有工单</el-radio>
      </el-radio-group>
      <div v-if="type === 1 && saveType === machinePartIssuedWayEnum.ADD_NEW_TICKET.V" style="margin-top: 15px">
        <!-- <pack-select v-model="selectBagId" :project-id="projectId" :packType="packType" style="width: 100%" /> -->
      </div>
    </template>
    <template #titleRight>
      <common-button @click="submitIt" :loading="submitLoading" size="mini" type="primary">保存</common-button>
    </template>
    <div class="head-container">
      <el-form style="display: flex; flex-wrap: wrap">
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
            :options="thickList"
            :dataStructure="{ key: 'name', label: 'name', value: 'name' }"
            clearable
            filterable
            allow-create
            type="other"
            class="filter-item"
            placeholder="请选择厚度"
            style="width: 160px"
          />
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
            value-format="YYYY-MM-DD"
            :disabled-date="disabledDate"
          />
        </el-form-item>
      </el-form>
    </div>
    <common-table :data="list" :data-format="dataFormat" :max-height="maxHeight" style="width: 100%">
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column :show-overflow-tooltip="true" prop="project" label="所属项目" min-width="120px" align="center" />
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
      <el-table-column prop="quantity" :show-overflow-tooltip="true" label="数量" width="80" align="center" />
    </common-table>
  </common-dialog>
</template>

<script setup>
import { save } from '@/api/mes/scheduling-manage/machine-part'
import { ElNotification, ElRadioGroup } from 'element-plus'
import { defineEmits, defineProps, ref } from 'vue'
// import { materialTypeEnum } from '@enum-ms/uploading-form'
import { componentTypeEnum, machinePartIssuedWayEnum } from '@enum-ms/mes'
import { manualFetchGroupsTree } from '@compos/mes/scheduling/use-scheduling-groups'
import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
import cutConfigSelect from '@/components-system/base/cut-config-select.vue'

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
  type: {
    type: Number
  }
})

const dataFormat = ref([['project', 'parse-project']])

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
const saveType = ref(machinePartIssuedWayEnum.NESTING_ISSUED.V)
// const crud = inject('crud')

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
  saveType.value = machinePartIssuedWayEnum.NESTING_ISSUED.V
  fetchGroups()
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
  try {
    submitLoading.value = true
    const _list = []
    props.list.forEach((v) => {
      v.needMachinePartLinkList.forEach((o) => {
        _list.push({
          productId: v.id,
          quantity: o.quantity,
          id: o.id,
          needSchedulingMonth: o.date
        })
      })
    })
    await save({
      ...schedulingGroups.value.obj[groupsId.value],
      material: material.value,
      thick: thick.value,
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
    handleClose()
    emit('success')
    askCompleteTime.value = undefined
    cutConfigId.value = undefined
    groupsId.value = undefined
    thick.value = undefined
    material.value = undefined
  } catch (error) {
    console.log('保存零件排产报错', error)
  } finally {
    submitLoading.value = false
  }
}
</script>
