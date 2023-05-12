<template>
  <common-drawer
    ref="drawerRef"
    :visible="crud.status.cu > 0"
    :before-close="crud.cancelCU"
    title="上传工艺文件"
    :show-close="true"
    :wrapper-closable="false"
    :close-on-click-modal="false"
    size="50%"
    custom-class="delivery-detail"
  >
    <template #titleRight>
      <common-button :loading="crud.status.cu === 2" type="primary" size="mini" @click="crud.submitCU">提交</common-button>
    </template>
    <template #content>
      <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="100px">
        <el-form-item label="工艺类型" prop="processType">
          <common-radio
            v-model="form.processType"
            :options="planProcessTypeEnum.ENUM"
            type="enum"
          />
        </el-form-item>
        <el-form-item label="文件类型" prop="boolSingleProject">
          <common-radio
            v-model="form.boolSingleProject"
            :options="processUseTypeEnum.ENUM"
            type="enum"
          />
        </el-form-item>
        <el-form-item label="所属项目" v-if="form.boolSingleProject" prop="projectId">
          <project-cascader v-model="form.projectId" clearable style="width: 270px;margin-bottom:10px;" placeholder="项目搜索" />
        </el-form-item>
        <el-form-item label="上传文件" prop="fileList">
          <upload-btn ref="uploadRef" btnType="warning" v-model:files="form.attachmentFiles" :file-classify="fileClassifyEnum.PLAN_ATT.V" :show-file-list="false" :accept="'.pdf'" :limit="1" @change="uploadFile"/>
        </el-form-item>
        <common-table
          ref="detailRef"
          border
          :data="form.fileList"
          :max-height="maxHeight-200"
          style="width: 100%"
          class="table-form"
          return-source-data
          :showEmptySymbol="false"
        >
          <el-table-column label="序号" type="index" align="center" width="50" />
          <el-table-column prop="file" label="文件" align="left">
            <template v-slot="scope">
              <span style="cursor: pointer; color: #409eff" @click="attachmentView(scope.row)">{{scope.row.file}}</span>
            </template>
          </el-table-column>
          <el-table-column prop="fileName" label="名称" align="center">
            <template v-slot="scope">
              <el-input
                class="input-border-none"
                v-model="scope.row.fileName"
                placeholder="名称"
                style="width: 100%;"
                size="small"
                maxlength="20"
                clearable
              />
            </template>
          </el-table-column>
          <el-table-column prop="remark" label="备注" align="center">
            <template v-slot="scope">
              <el-input
                class="input-border-none"
                v-model.trim="scope.row.remark"
                type="textarea"
                :autosize="{ minRows: 1, maxRows: 3 }"
                :maxlength="200"
                placeholder="备注"
                style="width:100%"/>
            </template>
          </el-table-column>
          <el-table-column label="操作" align="center" width="80">
            <template v-slot="scope">
              <common-button size="small" class="el-icon-delete" type="danger" @click="deleteRow(scope.$index)" />
            </template>
          </el-table-column>
        </common-table>
      </el-form>
      <showPdfAndImg v-if="pdfShow" :isVisible="pdfShow" :showType="'attachment'" :id="currentId" @close="pdfShow = false" />
    </template>
  </common-drawer>
</template>

<script setup>
import { ref } from 'vue'
import { regForm } from '@compos/use-crud'
// import { isNotBlank } from '@/utils/data-type'

import { fileClassifyEnum } from '@enum-ms/file'
import { processUseTypeEnum, planProcessTypeEnum } from '@enum-ms/plan'

// import UploadBtn from '@comp/file-upload/SingleFileUploadBtn'
import useMaxHeight from '@compos/use-max-height'
import projectCascader from '@comp-base/project-cascader.vue'
import { ElMessage } from 'element-plus'
import UploadBtn from '@comp/file-upload/UploadBtn'
import showPdfAndImg from '@comp-base/show-pdf-and-img.vue'

const formRef = ref()
const drawerRef = ref()
const uploadRef = ref()
const pdfShow = ref(false)
const currentId = ref()

const defaultForm = {
  boolSingleProject: processUseTypeEnum.NORMAL.V,
  processType: planProcessTypeEnum.WELD.V,
  projectId: undefined,
  fileList: []
}

const { CRUD, crud, form } = regForm(defaultForm, formRef)

const { maxHeight } = useMaxHeight(
  {
    mainBox: '.delivery-detail',
    extraBox: '.el-drawer__header',
    wrapperBox: '.el-drawer__body',
    paginate: false,
    minHeight: 300,
    navbar: false,
    clientHRepMainH: true
  },
  drawerRef
)

const rules = {
  boolSingleProject: { required: true, message: '请选择文件类型', trigger: 'change' },
  processType: { required: true, message: '请选择工艺类型', trigger: 'change' },
  projectId: { required: true, message: '请选择所属项目', trigger: 'change' },
  fileList: { required: true, message: '请上传文件', trigger: 'change' }
}

function deleteRow(index) {
  form.fileList.splice(index, 1)
}

CRUD.HOOK.beforeSubmit = (crud, form) => {
  if (crud.form.fileList.length <= 0) {
    ElMessage({ message: '请上传文件', type: 'error' })
    return false
  }
}

// 预览附件
function attachmentView(item) {
  currentId.value = item.attachmentId
  pdfShow.value = true
}

function uploadFile() {
  const file = crud.form.attachmentFiles[0].name
  const fileNameArr = file.split('.')
  let fileName = ''
  for (let i = 0; i < fileNameArr.length; i++) {
    if (i !== (fileNameArr.length - 1)) {
      if (i !== (fileNameArr.length - 2)) {
        fileName = fileName + fileNameArr[i] + '.'
      } else {
        fileName = fileName + fileNameArr[i]
      }
    }
  }
  crud.form.fileList.push({
    attachmentId: crud.form.attachmentFiles[0].id,
    fileName: fileName,
    file: file,
    remark: undefined
  })
  uploadRef.value.clearFiles()
  form.attachmentFiles = []
}

</script>

<style lang="scss" scoped>
.add-row-box{text-align: center;margin-top:10px;}
</style>
