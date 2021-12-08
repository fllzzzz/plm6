<template>
  <el-dialog
    append-to-body
    :close-on-click-modal="false"
    :before-close="crud.cancelCU"
    :visible="crud.status.edit === prepared"
    :title="crud.status.title"
    top="5vh"
    width="500px"
  >
    <template v-if="crud.status.edit === prepared" slot="title">
      <el-tooltip
        effect="light"
        :content="`辅材修改定义：\n
          1、辅材类型及编号无法修改；\n
          2、修改会生成一个变更记录和消息通知。`"
        placement="top"
      >
        <div style="display:inline-block;">
          <span v-text="crud.status.title" />
          <i class="el-icon-info" />
        </div>
      </el-tooltip>
    </template>
    <el-steps v-if="crud.status.edit === prepared" :active="stepActive" process-status="finish" finish-status="success" align-center style="margin-bottom:10px">
      <el-step v-for="(item, index) in stepList" :key="index" :title="item.title" />
    </el-steps>
    <div style="height:540px">
      <el-form v-show="stepActive === 0" ref="form" :model="form" :rules="rules" size="small" label-width="90px">
        <el-form-item label="辅材类别" prop="materialClass">
          <span v-if="crud.status.edit === prepared">{{ form.fullClassName }}</span>
        </el-form-item>
        <el-form-item label="编号" prop="serialNumber">
          <span v-if="crud.status.edit === prepared">{{ form.serialNumber }}</span>
        </el-form-item>
        <el-form-item label="颜色" prop="color">
          <el-input
            v-model="form.color"
            type="text"
            placeholder="填写辅材颜色"
            style="width: 270px;"
          />
        </el-form-item>
        <el-form-item label="规格" prop="specification">
          <el-input
            v-model="form.specification"
            type="text"
            placeholder="填写辅材规格"
            style="width: 270px;"
          />
        </el-form-item>
        <el-form-item label="备注" prop="remark">
          <el-input
            v-model.trim="form.remark"
            type="textarea"
            :autosize="{ minRows: 4, maxRows: 6}"
            placeholder="请填写备注"
            style="width: 320px;"
          />
        </el-form-item>
      </el-form>
      <change-reason-form v-show="stepActive === 1 || stepActive === 2" ref="reasonForm" />
    </div>
    <div slot="footer" class="dialog-footer">
      <el-button type="text" :disabled="crud.status.cu === 2" @click="crud.cancelCU">取消</el-button>
      <template v-if="crud.status.edit === prepared">
        <el-button v-if="stepActive != 0" :disabled="crud.status.cu === 2" type="warning" @click="stepActive--">上一步</el-button>
        <el-button v-if="stepList.length - 1 > stepActive" :disabled="crud.status.cu === 2" type="warning" @click="next">下一步</el-button>
        <el-button v-if="stepList.length - 1 <= stepActive" :loading="crud.status.cu === 2" type="primary" @click="next">提交</el-button>
      </template>
      <el-button v-else :loading="crud.status.cu === 2" type="primary" @click="crud.submitCU">确认</el-button>
    </div>
  </el-dialog>
</template>

<script>
import CRUD, { form } from '@crud/crud'
import changeReasonForm from '../../components/change-reason-form'

const defaultForm = {
  id: undefined,
  serialNumber: '',
  color: '',
  firstId: undefined,
  secondId: undefined,
  thirdId: undefined,
  remark: ''
}

const maxNubmer = 999999999

const stepList = [
  { title: '编辑辅材' },
  { title: '填写变更原因' }
]

export default {
  components: { changeReasonForm },
  mixins: [form(defaultForm)],
  data() {
    return {
      maxNubmer,
      stepList,
      stepActive: 0,
      prepared: CRUD.STATUS.PREPARED,
      rules: {
        serialNumber: [
          { required: true, message: '请填写辅材编号', trigger: 'blur' },
          { min: 1, max: 64, message: '长度在 1 到 64 个字符', trigger: 'blur' }
        ],
        specification: [{ max: 64, message: '不能超过64个字符', trigger: 'blur' }],
        color: [{ max: 24, message: '不能超过24个字符', trigger: 'blur' }],
        remark: [{ max: 500, message: '不能超过 500 个字符', trigger: 'blur' }]
      }
    }
  },
  methods: {
    [CRUD.HOOK.afterToCU]() {
      this.stepActive = 0
      this.$nextTick(() => {
        this.$refs.reasonForm && this.$refs.reasonForm.resetForm()
      })
    },
    // 下一步/确认
    async next() {
      if (this.stepActive === 0) {
        this.$refs['form'].validate((valid) => {
          if (valid) {
            this.stepActive++
          }
        })
      } else {
        try {
          const reason = await this.$refs.reasonForm.validForm()
          this.form.reason = {
            ...reason
          }
          this.stepActive++
          this.crud.submitCU()
        } catch (error) {
          console.log('填写变更原因', error)
        }
      }
    }
  }
}
</script>

<style rel="stylesheet/scss" lang="scss" scoped>
  /deep/ .el-input-number .el-input__inner {
    text-align: left;
  }
  /deep/ .el-dialog__body{
    padding: 10px 20px;

    .el-step {
      .el-step__icon {
        width: 20px;
        height: 20px;
        font-size: 12px;
      }
      .el-step__title {
        font-size: 13px;
      }
    }
  }
</style>
