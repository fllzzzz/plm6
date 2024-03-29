<template>
  <common-drawer
    append-to-body
    v-model="visible"
    top="10vh"
    width="600px"
    :before-close="handleClose"
    title="合同信息变更"
    :wrapper-closable="false"
    size="85%"
  >
    <template #title>
      <div class="dialog-title">
        <span class="title-left">合同信息变更</span>
        <el-tag v-if="auditStatus" size="medium" :type="auditStatus==auditTypeEnum.REJECT.V?'info':(auditStatus==auditTypeEnum.PASS.V?'success':'warning')">
          {{ auditStatus==auditTypeEnum.REJECT.V?'已驳回':(auditStatus==auditTypeEnum.PASS.V?'已通过':'审核中') }}
        </el-tag>
        <span style="position:absolute;right:20px;">
          <template v-if="auditStatus && visible">
            <common-button v-if="auditStatus==auditTypeEnum.AUDITING.V && showType==='audit'" size="small" type="info" @click="passConfirm(auditTypeEnum.REJECT.V)">驳回</common-button>
            <common-button v-if="auditStatus==auditTypeEnum.AUDITING.V && showType==='audit'" size="small" type="success" @click="passConfirm(auditTypeEnum.PASS.V)">通过</common-button>
          </template>
          <common-button size="small"  @click="handleClose">关闭</common-button>
        </span>
      </div>
    </template>
    <template #content>
      <el-tabs v-model="activeName">
        <el-tab-pane label="基础信息" name="baseInfo">
          <base-info ref="baseRef" class="tab-content" :detail="detailContractInfo" :originContractInfo="originContractInfo"/>
        </el-tab-pane>
        <el-tab-pane label="商务信息" name="businessInfo">
          <business-info ref="businessRef" class="tab-content" :detail="detailContractInfo" :originContractInfo="originContractInfo"/>
        </el-tab-pane>
        <el-tab-pane label="客户信息" name="customerInfo">
          <customer-info ref="customerRef" class="tab-content" :detail="detailContractInfo" :originContractInfo="originContractInfo"/>
        </el-tab-pane>
        <el-tab-pane label="项目成员" name="memberInfo">
          <members ref="memberRef" :checkedList="detailContractInfo.userlist" :detail="detailContractInfo" :originContractInfo="originContractInfo" />
        </el-tab-pane>
      </el-tabs>
    </template>
  </common-drawer>
</template>

<script setup>
import { defineProps, watch, defineEmits, ref } from 'vue'
import { auditTypeEnum, TechnologyTypeEnum } from '@enum-ms/contract'
import useVisible from '@compos/use-visible'
import { confirmContract } from '@/api/contract/project'
import { ElTabs, ElTabPane, ElNotification, ElMessageBox } from 'element-plus'
import { getChangeInfo } from '@/api/contract/change-audit-log'
import baseInfo from './base'
import businessInfo from './business'
import customerInfo from './customer'
import members from './members'
import { useStore } from 'vuex'
import { mapGetters } from '@/store/lib'

const { globalProjectId } = mapGetters(['globalProjectId'])
const store = useStore()

const props = defineProps({
  auditStatus: [Number, String],
  modelValue: {
    type: Boolean,
    require: true
  },
  showType: {
    type: String,
    default: undefined
  },
  detailInfo: {
    type: Object,
    default: () => {}
  }
})
const activeName = ref('baseInfo')
const emit = defineEmits(['success', 'update:modelValue'])
const { visible, handleClose } = useVisible({ emit, props })
const detailContractInfo = ref({})
const originContractInfo = ref({})
watch(
  () => visible.value,
  (val) => {
    if (val) {
      detailContractInfo.value = {}
      originContractInfo.value = {}
      if (props.detailInfo.id) {
        getInfo(props.detailInfo.id)
      }
      activeName.value = 'baseInfo'
    }
  },
  { deep: true, immediate: true }
)

async function getInfo(id) {
  try {
    const { projectDto, projectTempDto } = await getChangeInfo({ changeId: id })
    projectDto.projectContentName = projectDto.projectContentList.map(v => v.name)?.join('、')
    projectTempDto.projectContentName = projectTempDto.projectContentList.map(v => v.name)?.join('、')
    projectDto.relationUserId = projectDto.relationUserList?.map(v => v.id)?.join('、')
    projectTempDto.relationUserId = projectTempDto.relationUserList?.map(v => v.id)?.join('、')
    projectDto.relationUserName = projectDto.relationUserList?.map(v => v.name)?.join('、')
    projectTempDto.relationUserName = projectTempDto.relationUserList?.map(v => v.name)?.join('、')
    detailContractInfo.value = projectTempDto
    originContractInfo.value = projectDto
    detailContractInfo.value.enclosureInfo = {
      [TechnologyTypeEnum.STRUCTURE.V]: detailContractInfo.value.structureList || [],
      [TechnologyTypeEnum.PROFILED_PLATE.V]: detailContractInfo.value.profiledPlateList || [],
      [TechnologyTypeEnum.TRUSS_FLOOR_PLATE.V]: detailContractInfo.value.trussFloorPlateList || [],
      [TechnologyTypeEnum.PRESSURE_BEARING_PLATE.V]: detailContractInfo.value.pressureBearingPlateList || [],
      [TechnologyTypeEnum.SANDWICH_BOARD.V]: detailContractInfo.value.sandwichBoardList || []
    }
    originContractInfo.value.enclosureInfo = {
      [TechnologyTypeEnum.STRUCTURE.V]: originContractInfo.value.structureList || [],
      [TechnologyTypeEnum.PROFILED_PLATE.V]: originContractInfo.value.profiledPlateList || [],
      [TechnologyTypeEnum.TRUSS_FLOOR_PLATE.V]: originContractInfo.value.trussFloorPlateList || [],
      [TechnologyTypeEnum.PRESSURE_BEARING_PLATE.V]: originContractInfo.value.pressureBearingPlateList || [],
      [TechnologyTypeEnum.SANDWICH_BOARD.V]: originContractInfo.value.sandwichBoardList || []
    }
    detailContractInfo.value.userlist = detailContractInfo.value.relationUserIds ? detailContractInfo.value.relationUserIds.split(',') : []
    originContractInfo.value.userlist = originContractInfo.value.relationUserIds ? originContractInfo.value.relationUserIds.split(',') : []
  } catch (e) {
    console.log('获取变更信息', e)
  }
}

const inputValid = (val) => {
  if ((!val || !val.trim()) && val !== 0) {
    return '必填'
  }
  if (val.length > 200) {
    return '长度在 1 到 200 个字符'
  }
  return true
}

async function handleSuccess() {
  try {
    await store.dispatch('project/fetchUserProjects')
    await store.dispatch('project/fetchProjectTree')
    if (globalProjectId.value) {
      await store.dispatch('project/setProjectId', globalProjectId.value)
    }
  } catch (e) {
    console.log(e)
  }
}

async function passConfirm(val) {
  try {
    const title = val === auditTypeEnum.PASS.V ? '通过' : '驳回'
    const remarkValue = await ElMessageBox.prompt('请输入审核说明', title, {
      confirmButtonText: '确定',
      cancelButtonText: '取消',
      inputType: 'textarea',
      inputValidator: inputValid,
      type: 'warning'
    })
    const submitData = {
      auditStatus: val,
      id: props.detailInfo.id,
      remark: remarkValue.value
    }
    await confirmContract(submitData)
    ElNotification({ title: '提交成功', type: 'success' })
    if (val === auditTypeEnum.PASS.V) {
      handleSuccess()
    }
    emit('success')
    handleClose()
  } catch (error) {
    console.log('审核', error)
  }
}
</script>

<style rel="stylesheet/scss" lang="scss" scoped>
  ::v-deep(.el-input-number .el-input__inner) {
    text-align: left;
  }
  .title-left {
    display: flex;
    align-items: center;
    position: relative;
    padding-left: 10px;
    margin-right: 15px;
    box-sizing: border-box;
  }
  .title-left::before {
    content: "";
    width: 4px;
    height: 15px;
    border-radius: 10px;
    background: #1890ff;
    position: absolute;
    top: 50%;
    left: 0;
    transform: translateY(-50%);
}
</style>
