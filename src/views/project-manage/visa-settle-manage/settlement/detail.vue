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
      >下载结算单</export-button>
      <span v-if="props.status === reviewStatusEnum.UNREVIEWED.V && showType === 'audit'">
        <common-button :loading="submitLoading" size="mini" type="success" @click="handleSubmit(true)">确 签</common-button>
        <el-popconfirm title="确定要拒绝此条结算单吗？" @confirm="handleSubmit(false)">
          <template #reference>
            <common-button :loading="submitLoading" size="mini" type="danger">拒 绝</common-button>
          </template>
        </el-popconfirm>
      </span>
    </template>
    <template  #content>
      <el-form  v-loading="crud.detailLoading" ref="formRef" size="mini" label-width="100px" class="demo-form">
        <div class="rule-row">
          <el-form-item label="项目" prop="projectId">
            <span class="project-name">{{ projectNameFormatter(detail.project) }}</span>
          </el-form-item>
          <el-form-item label="" />
        </div>
        <div class="rule-row">
          <el-form-item label="签约单位" prop="contractSignBodyName">
            <div>{{ detail?.project?.contractSignBodyName }}</div>
          </el-form-item>
          <el-form-item label="发包单位" prop="customerUnit">
            <div>{{ detail?.project?.customerUnit }}</div>
          </el-form-item>
        </div>
        <div class="rule-row">
          <el-form-item label="签订日期" prop="signingDate">
            <div v-parse-time="{ val: detail?.project?.signingDate, fmt: '{y}-{m}-{d}' }" />
          </el-form-item>
          <el-form-item label="项目经理" prop="projectManagerName">
            <div>{{ detail?.project?.projectManagerName }}</div>
          </el-form-item>
        </div>
         <div class="rule-row">
          <el-form-item label="保证金额" prop="marginAmount">
            <div><span v-thousand="detail?.project?.marginAmount || 0" /><span v-if="detail?.project?.marginType">（{{ dict.label['margin_type'][detail?.project?.marginType] }}）</span></div>
          </el-form-item>
         <el-form-item label="合同额" prop="contractAmount">
            <div><span v-thousand="detail?.project?.contractAmount || 0" />（{{ digitUppercase(detail?.project?.contractAmount || 0) }}）</div>
          </el-form-item>
        </div>
        <div class="rule-row">
          <el-form-item label="签证额" prop="visaAmount">
            <div>{{ detail.visaAmount }}（{{ digitUppercase(detail.visaAmount) }}）</div>
          </el-form-item>
          <el-form-item label="违约金额" prop="breachAmount">
            <div>{{ detail.breachAmount }}（{{ digitUppercase(detail.breachAmount) }}）</div>
          </el-form-item>
        </div>
        <div class="rule-row">
          <el-form-item label="最终结算额" prop="settlementAmount">
            <div><span v-thousand="detail.settlementAmount" />（{{ digitUppercase(detail.settlementAmount) }}）</div>
          </el-form-item>
          <el-form-item label="大写">
            <span>{{ detail.settlementAmount?digitUppercase(detail.settlementAmount):'' }}</span>
          </el-form-item>
        </div>
        <div class="rule-row">
          <el-form-item label="已付款" prop="collectionAmount">
            <div><span v-thousand="detail?.project?.collectionAmount" />（{{ digitUppercase(detail?.project?.collectionAmount || 0) }}）</div>
          </el-form-item>
          <el-form-item label="欠款额" prop="debitAmount">
            <div><span v-thousand="debitAmount" />（{{ digitUppercase(debitAmount || 0) }}）</div>
          </el-form-item>
        </div>
        <div class="rule-row">
          <el-form-item label="已开票" prop="invoiceAmount">
            <div><span v-thousand="detail?.project?.invoiceAmount" />（{{ digitUppercase(detail?.project?.invoiceAmount || 0) }}）</div>
          </el-form-item>
          <el-form-item label="应补发票" prop="debitInvoice">
            <div><span v-thousand="debitInvoice" />（{{ digitUppercase(debitInvoice || 0) }}）</div>
          </el-form-item>
        </div>
        <div class="rule-row">
          <el-form-item label="备注" prop="remark">
            <div>{{ detail.remark }}</div>
          </el-form-item>
        </div>
        <div class="item-center">
          <el-form-item label="确签附件" v-if="showType==='audit' || props.status === reviewStatusEnum.PASS.V">
            <upload-btn v-if="props.status === reviewStatusEnum.UNREVIEWED.V" ref="uploadRef" v-model:files="detail.files" :file-classify="fileClassifyEnum.CONTRACT_VISA.V" :accept="'.pdf,.png,.jpg,.jpeg'"/>
            <template v-else>
              <div v-for="item in detail.files" :key="item.id">{{item.name}}
                <export-button :params="{id: item.id}" size="mini"/>
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
          </el-form-item>
        </div>
      </el-form>
    </template>
  </common-drawer>
</template>

<script setup>
import { downloadSettle as download } from '@/api/project-manage/visa-settle-manage'
import { check } from '@/api/contract/sales-manage/visa-manage'
import { ref, defineProps, defineEmits, computed } from 'vue'

import { fileClassifyEnum } from '@enum-ms/file'
import { projectNameFormatter } from '@/utils/project'
import { reviewStatusEnum } from '@enum-ms/common'
import { digitUppercase } from '@data-type/number'

import { regDetail } from '@compos/use-crud'
import useDict from '@compos/store/use-dict'
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
  return props.status === reviewStatusEnum.UNREVIEWED.V ? '结算单待确签' : '结算单详情'
})

// 欠款额
const debitAmount = computed(() => {
  return (detail.settlementAmount || 0) - (detail?.project?.collectionAmount || 0)
})

// 应补发票
const debitInvoice = computed(() => {
  return (detail.settlementAmount || 0) - (detail?.project?.invoiceAmount || 0)
})

const formRef = ref()
const submitLoading = ref(false)

const dict = useDict(['margin_type'])
const { crud, detail, CRUD } = regDetail()

// 详情加载后
CRUD.HOOK.beforeDetailLoaded = async (crud) => {
  try {
    detail.files = detail.attachments || []
  } catch (error) {
    crud.notify('获取结算单详情失败', CRUD.NOTIFICATION_TYPE.ERROR)
  }
}

// 结算单确签
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
    crud.notify('提交结算单确签信息', CRUD.NOTIFICATION_TYPE.ERROR)
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
