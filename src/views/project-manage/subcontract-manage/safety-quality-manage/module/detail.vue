<template>
  <common-drawer
    ref="detailRef"
    append-to-body
    :close-on-click-modal="false"
    :before-close="handleClose"
    v-model="visible"
    title="问题报告"
    :wrapper-closable="false"
    size="50%"
  >
    <template #titleAfter>
      <el-tag :type="detailInfo.auditStatus===auditTypeEnum.PASS.V?'success':(detailInfo.auditStatus===auditTypeEnum.REJECT.V?'warning':'')" >{{ auditTypeEnum.VL[detailInfo.auditStatus] }}</el-tag>
    </template>
    <template #titleRight>
      <export-button
        v-if="checkPermission(permission.download)"
        :params="detailInfo.id"
        :fn="download"
      >下载问题报告</export-button>
      <el-popconfirm
        confirm-button-text="确定"
        cancel-button-text="取消"
        icon="el-icon-info"
        icon-color="red"
        :title="`确认关闭问题吗?`"
        v-if="showType==='close'"
        @confirm="closeProblem"
      >
        <template #reference>
          <common-button v-loading="submitLoading" size="mini" type="primary">关闭问题</common-button>
        </template>
      </el-popconfirm>
      <common-button v-loading="submitLoading" v-if="showType==='audit'" size="mini" type="info" @click="passConfirm(auditTypeEnum.REJECT.V)">驳回</common-button>
      <common-button v-loading="submitLoading" v-if="showType==='audit'" size="mini" type="success" @click="passConfirm(auditTypeEnum.PASS.V)">通过</common-button>
    </template>
    <template #content>
      <el-form ref="formRef" size="small" label-width="130px">
        <el-form-item label="所属项目">
          <span class="project-name">{{ projectNameFormatter(detailInfo?.project) }}</span>
        </el-form-item>
        <div style="display:flex;">
          <el-form-item label="分包单位" style="width:50%;">
            <span>{{ detailInfo?.supplierName }}</span>
          </el-form-item>
          <el-form-item label="分类" style="width:50%;">
            <span>{{ detailInfo?.subcontractClassName }}</span>
          </el-form-item>
        </div>
         <div style="display:flex;">
          <el-form-item label="日期" style="width:50%;">
            <span v-parse-time="{ val: detailInfo?.problemDate, fmt: '{y}-{m}-{d}' }" />
          </el-form-item>
          <el-form-item label="问题分类" style="width:50%;">
            <span>{{ detailInfo?.problemTypeName }}</span>
          </el-form-item>
        </div>
        <el-form-item label="违约金">
          <span v-thousand="detailInfo?.penalty" v-empty-text />
        </el-form-item>
        <div style="display:flex;">
          <el-form-item label="整改前" style="width:50%;">
            <div class="imgs-box">
              <el-image
                v-for="url in detailInfo?.beforeChangeImgs"
                :preview-src-list="detailInfo?.beforeImgSrc"
                :initial-index="1"
                :key="url.id"
                :src="url.tinyImageUrl"
                lazy
                style="margin:0 2px;"
              ></el-image>
            </div>
          </el-form-item>
          <el-form-item label="整改前描述" style="width:50%;">
            <span>{{ detailInfo.remark || '-' }}</span>
          </el-form-item>
        </div>
        <div style="display:flex;">
          <el-form-item label="整改后" style="width:50%;">
            <div class="imgs-box">
              <el-image
                v-for="url in detailInfo?.afterChangeImgs"
                :preview-src-list="detailInfo?.afterImgSrc"
                :initial-index="1"
                :key="url.id"
                :src="url.tinyImageUrl"
                lazy
                style="margin:0 2px;"
              ></el-image>
            </div>
          </el-form-item>
          <el-form-item label="整改后描述" style="width:50%;">
            <span>{{ detailInfo.qhseChange?.problemDesc || '-' }}</span>
          </el-form-item>
        </div>
      </el-form>
    </template>
  </common-drawer>
</template>

<script setup>
import { problemAudit, download, close } from '@/api/project-manage/quality-problem-manage'
import { ref, defineProps, defineEmits } from 'vue'
import { ElNotification } from 'element-plus'

import checkPermission from '@/utils/system/check-permission'
import { projectNameFormatter } from '@/utils/project'
import { auditTypeEnum } from '@enum-ms/contract'
import useVisible from '@compos/use-visible'

import ExportButton from '@comp-common/export-button/index.vue'

const emit = defineEmits(['success', 'update:modelValue'])
const { visible, handleClose } = useVisible({ emit, props })

const props = defineProps({
  detailInfo: {
    type: Object,
    default: () => {}
  },
  permission: {
    type: Object,
    default: () => {}
  },
  showType: {
    type: String,
    default: 'detail'
  }
})

const submitLoading = ref(false)
const detailRef = ref()

async function closeProblem() {
  try {
    await close(props.detailInfo.qhseChange.id)
    submitLoading.value = false
    ElNotification({ title: '问题关闭成功', type: 'success' })
    emit('success')
    handleClose()
  } catch (error) {
    submitLoading.value = false
    console.log('问题关闭失败', error)
  }
}

async function passConfirm(val) {
  submitLoading.value = true
  const msg = val === auditTypeEnum.PASS.V ? '通过成功' : '驳回成功'
  try {
    const submitForm = {
      auditStatus: val,
      id: props.detailInfo.qhseChange?.id
    }
    await problemAudit(submitForm)
    submitLoading.value = false
    ElNotification({ title: '审核' + msg, type: 'success' })
    emit('success')
    handleClose()
  } catch (error) {
    submitLoading.value = false
    console.log('问题审核失败', error)
  }
}
</script>
<style lang="scss" scoped>
.imgs-box {
  & > .el-image {
    width: 50px;
    height: 40px;
    border: 2px solid #dcdfe6;
    border-radius: 6px;
    background-color: white;
    cursor: pointer;
    + .el-image {
      margin-left: -40px;
    }
  }
}
</style>
