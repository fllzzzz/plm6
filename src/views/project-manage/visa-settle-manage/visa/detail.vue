<template>
  <common-drawer
    append-to-body
    :close-on-click-modal="false"
    :before-close="crud.cancelDetail"
    :visible="crud.detailVisible"
    :title="detailTitle"
    :show-close="true"
    size="50%"
  >
    <template #titleRight>
      <export-button
        v-permission="crud.permission.download"
        :params="detail.id"
        :fn="download"
      >下载签证单</export-button>
      <span v-if="props.status === reviewStatusEnum.UNREVIEWED.V && showType === 'audit'">
        <common-button :loading="submitLoading" size="mini" type="success" @click="handleSubmit(true)">确 签</common-button>
        <el-popconfirm title="确定要拒绝此条签证单吗？" @confirm="handleSubmit(false)">
          <template #reference>
            <common-button :loading="submitLoading" size="mini" type="danger">拒 绝</common-button>
          </template>
        </el-popconfirm>
      </span>
    </template>
    <template  #content>
      <el-form  v-loading="crud.detailLoading" ref="formRef" size="mini" label-width="100px" class="demo-form">
        <div class="rule-row">
          <el-form-item label="项目" prop="project">
            <span class="project-name">{{ projectNameFormatter(detail.project) }}</span>
          </el-form-item>
          <el-form-item label="签证单号" prop="serialNumber">
            <div>{{ detail.serialNumber }}</div>
          </el-form-item>
        </div>
        <div class="rule-row">
          <el-form-item label="签证日期" prop="visaDate">
            <div v-parse-time="{ val: detail.visaDate, fmt: '{y}-{m}-{d}' }" />
          </el-form-item>
          <el-form-item label="申请人" prop="userName">
            <div>{{ detail.userName }}</div>
          </el-form-item>
        </div>
        <div class="rule-row">
          <el-form-item label="施工单位" prop="contractSignBodyName">
            <div>{{ detail?.project?.contractSignBodyName }}</div>
          </el-form-item>
          <el-form-item label="发包单位" prop="customerUnit">
            <div>{{ detail?.project?.customerUnit }}</div>
          </el-form-item>
        </div>
        <div class="rule-row">
          <el-form-item label="签证原因" prop="visaDate">
            <div>{{ detail.reasonName }}</div>
          </el-form-item>
          <el-form-item label="签证额" prop="userName">
            <div>{{ detail.visaAmount }}（{{ digitUppercase(detail.visaAmount) }}）</div>
          </el-form-item>
        </div>
        <div class="rule-row">
          <el-form-item label="签证备注" prop="remark">
            <div>{{ detail.remark }}</div>
          </el-form-item>
        </div>
        <div class="rule-row">
          <el-form-item label="监理单位" prop="supervisionUnit">
            <div>{{ detail.supervisionUnit }}</div>
          </el-form-item>
        </div>
        <div class="rule-row">
          <el-form-item label="设计单位" prop="designUnit">
            <div>{{ detail.designUnit }}</div>
          </el-form-item>
        </div>
        <!-- <div class="rule-row">
          <el-form-item label="政府监管" prop="governmentRegulation">
            <div>{{ detail.governmentRegulation }}</div>
          </el-form-item>
        </div> -->
        <div class="item-center">
          <el-form-item label="确签附件" v-if="showType==='audit' || props.status === reviewStatusEnum.PASS.V">
            <upload-btn v-if="props.status === reviewStatusEnum.UNREVIEWED.V" ref="uploadRef" v-model:files="detail.files" :file-classify="fileClassifyEnum.CONTRACT_VISA.V" :accept="'.pdf,.png,.jpg,.jpeg'"/>
            <template v-else>
              <div v-for="item in detail.files" :key="item.id">{{item.name}}
                <export-button :params="{id: item.id}" size="mini" />
              </div>
            </template>
          </el-form-item>
          <el-form-item label="确签备注" v-if="showType==='audit' || props.status === reviewStatusEnum.PASS.V">
            <el-input
              v-if="props.status === reviewStatusEnum.UNREVIEWED.V"
              v-model.trim="detail.attachmentRemark"
              type="textarea"
              :autosize="{ minRows: 2, maxRows: 4 }"
              placeholder="请填写确签备注"
              maxlength="200"
              show-word-limit
            />
            <span v-else>{{detail.attachmentRemark}}</span>
          </el-form-item>
        </div>
      </el-form>
    </template>
  </common-drawer>
</template>

<script setup>
import { download } from '@/api/contract/sales-manage/visa-manage'
import { check } from '@/api/project-manage/visa-settle-manage'
import { ref, defineProps, defineEmits, computed } from 'vue'

import { fileClassifyEnum } from '@enum-ms/file'
import { projectNameFormatter } from '@/utils/project'
import { reviewStatusEnum } from '@enum-ms/common'
import { digitUppercase } from '@data-type/number'

import { regDetail } from '@compos/use-crud'
import UploadBtn from '@comp/file-upload/UploadBtn'
import ExportButton from '@comp-common/export-button/index.vue'

const emit = defineEmits(['success'])

const props = defineProps({
  status: { // 签证单状态
    type: Number,
    default: undefined
  },
  showType: {
    type: String,
    default: undefined
  }
})

// 标题
const detailTitle = computed(() => {
  return props.status === reviewStatusEnum.UNREVIEWED.V ? '签证单待确签' : '签证单详情'
})

const formRef = ref()
const submitLoading = ref(false)

const { crud, detail, CRUD } = regDetail()

// 详情加载后
CRUD.HOOK.beforeDetailLoaded = async (crud) => {
  try {
    detail.files = detail.attachments || []
  } catch (error) {
    crud.notify('获取签证单详情失败', CRUD.NOTIFICATION_TYPE.ERROR)
  }
}

// 签证单确签
async function handleSubmit(status) {
  submitLoading.value = true
  try {
    const params = {
      id: detail.id
    }
    if (status) {
      if (!detail.files.length) {
        crud.notify('请上传确签附件', CRUD.NOTIFICATION_TYPE.WARNING)
        return
      }
      params.status = reviewStatusEnum.PASS.V
      params.attachmentRemark = detail.attachmentRemark
      params.attachmentIds = detail.files.map(f => f.id)
    } else {
      params.status = reviewStatusEnum.REFUSE.V
    }
    await check(params)
    crud.cancelDetail()
    emit('success')
  } catch (error) {
    crud.notify('提交签证单确签信息', CRUD.NOTIFICATION_TYPE.ERROR)
  } finally {
    submitLoading.value = false
  }
}
</script>

<style rel="stylesheet/scss" lang="scss" scoped>
.form {
  padding:  0px 10px 10px;
}
.demo-form .rule-row {
  display: flex;
  margin-bottom: 10px;
}
.demo-form .rule-row:last-child {
  margin-bottom: 0px;
}
.demo-form .el-form-item {
  flex: 1;
  width: 50%;
  margin-right: 10px;
}
.demo-form .el-upload__tip {
  padding-left: 15px;
}
.item-center {
  margin: 10px;
}
</style>
