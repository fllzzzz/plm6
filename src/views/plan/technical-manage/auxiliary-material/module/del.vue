<template>
  <div v-permission="permission.changeQuantity" style="display:inline-block">
    <el-button type="danger" icon="el-icon-s-operation" size="mini" @click.stop="toDelete" />
    <el-dialog
      append-to-body
      :close-on-click-modal="false"
      :visible.sync="visible"
      top="5vh"
      width="500px"
      @closed="handleClosed"
    >
      <!-- TODO: 提示 -->
      <template slot="title">
        <el-tooltip
          effect="light"
          :content="`辅材数量变更定义：\n
                1. 清单数量：修改辅材数量时，数量不能小于已进入出库的辅材数量。\n
                * 上诉所有的操作中除数量增加外，均会使被修改辅材进入暂停状态，需通知生产部门进行对应的任务变动`"
          placement="right"
        >
          <div style="display:inline-block;">
            <span v-text="'辅材数量变更'" />
            <i class="el-icon-info" />
          </div>
        </el-tooltip>
      </template>
      <el-steps :active="stepActive" process-status="finish" finish-status="success" align-center style="margin-bottom:10px">
        <el-step v-for="(item, index) in stepList" :key="index" :title="item.title" />
      </el-steps>
      <div style="height:540px">
        <el-form v-show="stepActive === 0" ref="form" :inline="true" :model="form" :rules="rules" size="small" label-width="120px">
          <el-form-item label="物料类别">
            <span style="display:inline-block;width:100px">{{ data.thirdName }}</span>
          </el-form-item>
          <el-form-item label="辅材数量">
            {{ data.quantity }}
          </el-form-item>
          <el-form-item label="编号">
            <span style="display:inline-block;width:100px">{{ data.serialNumber }}</span>
          </el-form-item>
          <el-form-item label="出库数量">
            <span style="color:red">{{ data.outboundQuantity }}</span>
          </el-form-item>
          <el-tag class="tag" effect="light" size="small">
            <span v-text="'清单数量'" />
          </el-tag>
          <!-- 数量变更 -->
          <el-form-item label="数量" prop="modifiedquantity">
            <template slot="label">
              数量
            </template>
            <el-input-number
              v-model.number="form.modifiedquantity"
              :min="data.outboundQuantity"
              :max="999999999"
              :step="1"
              :placeholder="`请填写辅材数量`"
              controls-position="right"
              style="width: 270px;"
            />
          </el-form-item>
        </el-form>
        <change-reason-form v-show="stepActive === 1 || stepActive === 2" ref="reasonForm" />
      </div>
      <div slot="footer" class="dialog-footer">
        <el-button type="text" :disabled="loading" @click="cancel">取消</el-button>
        <el-button v-if="stepActive != 0" :disabled="loading" type="warning" @click="stepActive--">上一步</el-button>
        <el-button v-if="stepList.length - 1 > stepActive" :disabled="loading" type="warning" @click="next">下一步</el-button>
        <el-button v-if="stepList.length - 1 <= stepActive" :loading="loading" type="primary" @click="next">提交</el-button>
      </div>
    </el-dialog>
  </div>
</template>

<script>
import { changeQuantity } from '@/api/mes-plan/technical-manage/auxiliary-material'
import changeReasonForm from '../../components/change-reason-form'

const stepList = [
  { title: '填写变更内容' },
  { title: '填写变更原因' }
]

export default {
  components: { changeReasonForm },
  props: {
    data: {
      type: Object,
      required: true
    },
    permission: {
      type: Object,
      required: true
    },
    projectId: {
      type: [Number, String],
      required: true
    }
  },
  data() {
    return {
      stepList,
      stepActive: 0,
      visible: false,
      loading: false,
      form: {},
      currentMaterial: {},
      rules: {
        modifiedquantity: [{ required: true, message: '请填写数量', trigger: 'blur', type: 'number' }]
      }
    }
  },
  methods: {
    cancel() {
      this.visible = false
    },
    toDelete() {
      this.visible = true
      this.formAssignment()
    },
    formAssignment() {
      // 重置表单内容
      this.form = {
        id: this.data.id,
        monomerId: undefined,
        areaId: undefined,
        modifiedquantity: this.data.quantity // 修改的数量
      }
    },
    handleClosed() {
      this.resetForm()
    },
    resetForm() {
      this.stepActive = 0
      this.$refs.form.resetFields()
      this.$refs.reasonForm.resetForm()
    },
    materialChange(data) {
      this.currentMaterial = data
    },
    // 下一步/确认
    async next() {
      if (this.stepActive === 0) {
        this.$refs['form'].validate((valid) => {
          const validEdit = this.validEdit()
          if (valid && validEdit) {
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
          this.submit(this.form)
        } catch (error) {
          console.log('填写变更原因', error)
        }
      }
    },
    validEdit() {
      const numberChange = this.form.modifiedquantity !== this.data.quantity
      if (numberChange) {
        return true
      }
      this.$message.warning({ message: '未做改动', showClose: true })
      return false
    },
    async submit(data) {
      try {
        this.loading = true
        await changeQuantity(data)
        this.$emit('success', data)
      } catch (error) {
        console.log('辅材数量变更', error)
      } finally {
        this.loading = false
        this.cancel()
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
  .tag {
      display: flex;
      flex-direction: row;
      justify-content: center;
      align-content: center;
      font-size: 14px;
      margin-bottom: 10px;
  }
</style>
